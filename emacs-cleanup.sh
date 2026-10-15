#!/bin/bash
# Generated: 2026-10-15T14:15:00Z

FIXED_TARGETS="emacs-cleanup-target.txt"
USER_TARGETS="emacs-cleanup-userdefined.txt"
COMBINED_LIST="/tmp/emacs-cleanup-combined-all.txt"
RM_F="echo rm -f"

echo "========================================================================"
echo " Emacs Dual-Source Cache & Temporary Files Cleanup Script (Secure-v3)"
echo "========================================================================"

# 1. 安全対策：Emacs稼働チェック
if pgrep -x "emacs" > /dev/null; then
    echo "[ERROR] Emacsプロセスがまだ稼働しています。"
    echo "        安全な一掃のため、一度Emacsを完全に終了してから再実行してください。"
    exit 1
fi

# 2. 定義ファイルの存在確認
if [ ! -f "$FIXED_TARGETS" ]; then
    echo "[ERROR] 固定対象定義ファイル '$FIXED_TARGETS' が見つかりません。"
    exit 1
fi

# 3. リストの合成
"$RM_F" "$COMBINED_LIST"
cat "$FIXED_TARGETS" > "$COMBINED_LIST"

if [ -f "$USER_TARGETS" ]; then
    echo "[INFO] ユーザー定義ファイル '$USER_TARGETS' を統合します。"
    cat "$USER_TARGETS" >> "$COMBINED_LIST"
fi

echo "[INFO] 安全性検査およびクリーンアップ処理を開始します..."

# 4. 合成リストを1行ずつ読み込み、安全に削除を実行
while IFS= read -r line || [ -n "$line" ]; do

    # ========================================================================
    # 【バグ修正】case文の#コメント誤認を完全に回避する if 構造への置き換え
    # ========================================================================
    # 1. 空行、または2. 先頭が「#」で始まるコメント行であるかを安全に判定します。
    if [ -z "$line" ] || [[ "$line" == \#* ]]; then
        continue
    fi

    # パスを展開（~ を絶対パスへ変換）
    eval expanded_path="$line"

    if [ -d "$expanded_path" ]; then
        # 【防御ガード1】 ホーム、ルート、重要な基本ディレクトリの完全破壊を防ぐ
        if [ "$expanded_path" = "$HOME" ] || [ "$expanded_path" = "/" ] || [ "$expanded_path" = "$HOME/" ]; then
            echo " -> [安全ブロック] 危険なベースパスのため拒否: $expanded_path"
            continue
        fi

        # 【防御ガード2】 大切な資産（.git, .org, .md）を保持するディレクトリを完全に保護
        if find "$expanded_path" -maxdepth 4 \( -name ".git" -o -name "*.org" -o -name "*.md" \) -print -quit | grep -q .; then
            echo " -> [資産保護ブロック] 配下に .git/.org/.md の存在を検知したため、削除をスキップしました: $expanded_path"
            continue
        fi

        echo " -> 削除中: $expanded_path"
        rm -rf "$expanded_path"

        # システム一時領域の器のみ再構築
        if [ "$expanded_path" = "/tmp/emacs-temporary/" ]; then
            mkdir -p "$expanded_path"
            chmod 700 "$expanded_path"
        fi
    else
        echo " -> スキップ (存在しません): $expanded_path"
    fi
done < "$COMBINED_LIST"

# 一時合成ファイルのクリーンアップ
"$RM_F" "$COMBINED_LIST"

echo "------------------------------------------------------------------------"
echo "[SUCCESS] 安全基準を満たした不要キャッシュ群の一掃が完了しました。"
echo "========================================================================"
