package com.project.trash.member.request;

import com.project.trash.common.request.PageRequest;

import lombok.Getter;
import lombok.Setter;

/**
 * 등록한 리뷰 목록 조회
 */
@Getter
@Setter
public class MemberReviewListRequest extends PageRequest {

  /**
   * 회원 일련번호
   */
  private Long memberSeq;
}
