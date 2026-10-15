#!/usr/bin/env bash

# Generated: a3b92f7c-7d9a-4c12-b91c-f2b3e8da4ea2 | 2026-10-15T15:36:00Z

set -euo pipefail

# 設定ファイルの定義
TARGET_FILE="emacs-cleanup-target.txt"

DRY_RUN=false

# 引数の解析 (--dry-run のチェック)
for arg in "$@"; do
    if [[ "$arg" == "--dry-run" ]]; then
        DRY_RUN=true
        echo "=== DRY-RUN MODE: 実際の削除は行われません ==="
    fi
done

# --- 事前チェック: Emacsプロセスの稼働確認 ---
if pgrep -x "emacs" > /dev/null; then
    echo "エラー: Emacsプロセスが稼働中です。" >&2
    echo "データを破損する恐れがあるため、Emacsを完全に終了してから再実行してください。" >&2
    exit 1
fi

# ターゲット設定ファイルの存在確認
if [[ ! -f "$TARGET_FILE" ]]; then
    echo "エラー: $TARGET_FILE が見つかりません。" >&2
    exit 1
fi

# メイン処理のループ
while IFS= read -r line || [[ -n "$line" ]]; do
    # 空行やコメント行（#で始まる行）をスキップ
    [[ -z "$line" || "$line" =~ ^[[:space:]]*# ]] && continue

    # 前後の空白や末尾のスラッシュを削除してクリーンなパスを生成
    target_item=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' -e 's|/*$||')

    # パス内のチルダ (~) を現在のホームディレクトリ ($HOME) に置換・展開
    if [[ "$target_item" == "~"* ]]; then
        full_path_pattern="${target_item/#\~/$HOME}"
    else
        full_path_pattern="$target_item"
    fi

    # ワイルドカード（*, ?など）が含まれているかを判定し、パスを展開
    # 展開結果を配列に入れる（ファイルが存在しない場合はそのままパターン文字列が残るのを防ぐため nullglob を一時有効化）
    shopt -s nullglob
    expanded_paths=($full_path_pattern)
    shopt -u nullglob

    # 展開したパスが空（＝合致するファイル・フォルダが1つもない）ならスキップ
    if [[ ${#expanded_paths[@]} -eq 0 ]]; then
        continue
    fi

    # 展開された各パスに対して処理を実行
    for full_path in "${expanded_paths[@]}"; do
        # 対象が実際に存在するか確認
        if [[ ! -e "$full_path" ]]; then
            continue
        fi

        # --- 保護ルールチェック (単なるパス連結による直下チェック) ---
        if [[ -d "$full_path" ]]; then
            if [[ -e "$full_path/.git" || -e "$full_path/.org" || -e "$full_path/.md" ]]; then
                echo "保護: ${full_path} の直下に保護対象 (.git/.org/.md) が検出されたためスキップします。"
                continue
            fi
        fi

        # --- 削除またはドライランの実行 ---
        if [[ "$DRY_RUN" == true ]]; then
            echo "rm -rf \"$full_path\""
        else
            echo "削除中: $full_path"
            rm -rf "$full_path"
        fi
    done

done < "$TARGET_FILE"

if [[ "$DRY_RUN" == true ]]; then
    echo "=== DRY-RUN 終了 ==="
else
    echo "クリーンアップが完了しました。"
fi

