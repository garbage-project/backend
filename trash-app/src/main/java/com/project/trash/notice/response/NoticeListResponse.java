package com.project.trash.notice.response;

import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.notice.domain.Notice;

import lombok.Getter;
import lombok.Setter;

/**
 * 공지 목록 응답
 */
@Getter
@Setter
public class NoticeListResponse {

  /**
   * 공지 일련번호
   */
  private Long noticeSeq;
  /**
   * 제목
   */
  private String title;
  /**
   * 내용
   */
  private String content;
  /**
   * 등록일시
   */
  private String createdAt;

  public NoticeListResponse(Notice notice) {
    this.noticeSeq = notice.getNoticeSeq();
    this.title = notice.getTitle();
    this.content = notice.getContent();
    this.createdAt = DateTimeUtils.convertToString(notice.getCreatedAt(), DateTimeUtils.DISPLAY_DEFAULT_DATETIME);
  }
}
