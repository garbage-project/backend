package com.project.trash.notice.request;

import lombok.Getter;

/**
 * 공지 수정 요청
 */
@Getter
public class NoticeModifyRequest {

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
}
