package com.project.trash.notice.response;

import com.project.trash.common.utils.DateTimeUtils;

import java.time.LocalDateTime;

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
   * 유효여부
   */
  private String valid;
  /**
   * 등록일자
   */
  private String createdDate;

  public NoticeListResponse(Long noticeSeq, String title, String content, String valid, LocalDateTime createdAt) {
    this.noticeSeq = noticeSeq;
    this.title = title;
    this.content = content;
    this.valid = valid;
    this.createdDate = DateTimeUtils.convertToString(createdAt, DateTimeUtils.DEFAULT_DATE);
  }
}
