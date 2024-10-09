package com.project.trash.notice.response;

import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.notice.domain.Notice;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Schema(title = "공지 목록 조회 응답")
public class NoticeListResponse {

  @Schema(description = "공지 ID", example = "1")
  private Long noticeId;

  @Schema(description = "제목", example = "시설물 사용에 대한 공지사항입니다.")
  private String title;

  @Schema(description = "공지 내용", example = "청결하게 사용해주세요!")
  private String content;

  @Schema(description = "공지 등록일시", example = "2024-09-01 10:30:00")
  private String createdAt;

  public NoticeListResponse(Notice notice) {
    this.noticeId = notice.getNoticeId();
    this.title = notice.getTitle();
    this.content = notice.getContent();
    this.createdAt = DateTimeUtils.convertToString(notice.getCreatedAt(), DateTimeUtils.DISPLAY_DEFAULT_DATETIME);
  }
}
