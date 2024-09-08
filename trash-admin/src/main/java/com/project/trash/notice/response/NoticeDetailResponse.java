package com.project.trash.notice.response;

import com.project.trash.common.domain.enums.Valid;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.notice.domain.Notice;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

/**
 * 공지 상세 응답
 */
@Getter
@Setter
@Schema(title = "공지 상세 조회 응답")
public class NoticeDetailResponse {

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
   * 등록일시
   */
  @Schema(description = "공지 등록일시", example = "2024-09-01 10:30:00")
  private String createdAt;

  public NoticeDetailResponse(Notice notice) {
    this.noticeSeq = notice.getNoticeSeq();
    this.title = notice.getTitle();
    this.content = notice.getContent();
    this.valid = notice.getValid().equals(Boolean.TRUE) ? Valid.TRUE.getCode() : Valid.FALSE.getCode();
    this.createdAt = DateTimeUtils.convertToString(notice.getCreatedAt(), DateTimeUtils.DISPLAY_DEFAULT_DATETIME);
  }
}
