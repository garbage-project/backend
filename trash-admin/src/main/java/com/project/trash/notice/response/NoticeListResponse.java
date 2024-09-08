package com.project.trash.notice.response;

import com.project.trash.common.utils.DateTimeUtils;

import java.time.LocalDateTime;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

/**
 * 공지 목록 응답
 */
@Getter
@Setter
@Schema(title = "공지 목록 조회 응답")
public class NoticeListResponse {

  /**
   * 공지 일련번호
   */
  @Schema(description = "공지 일련번호", example = "1")
  private Long noticeSeq;
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
  /**
   * 등록일자
   */
  @Schema(description = "공지 등록일자", example = "2024-09-01")
  private String createdDate;

  public NoticeListResponse(Long noticeSeq, String title, String content, String valid, LocalDateTime createdAt) {
    this.noticeSeq = noticeSeq;
    this.title = title;
    this.content = content;
    this.valid = valid;
    this.createdDate = DateTimeUtils.convertToString(createdAt, DateTimeUtils.DEFAULT_DATE);
  }
}
