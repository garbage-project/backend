package com.project.trash.notice.request;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 공지 등록 요청
 */
@Getter
public class NoticeEntryRequest {

  /**
   * 제목
   */
  @Schema(description = "제목", example = "시설물 사용에 대한 공지사항입니다.")
  private String title;
  /**
   * 내용
   */
  @Schema(description = "공지 내용", example = "청결하게 사용해주세요!")
  private String content;
  /**
   * 노출여부
   */
  @Schema(description = "공지 노출여부(Y - 노출, N - 미노출)", example = "Y")
  private String valid;
}
