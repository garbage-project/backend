package com.project.trash.utils;

import com.project.trash.member.domain.MemberDetail;

import org.springframework.security.core.context.SecurityContextHolder;

import lombok.experimental.UtilityClass;

/**
 * 회원 유틸
 */
@UtilityClass
public class MemberUtils {

  /**
   * 로그인 회원 정보 조회
   *
   * @return 회원 정보
   */
  public MemberDetail getMember() {
    if (SecurityContextHolder.getContext().getAuthentication().getPrincipal() instanceof MemberDetail member) {
      return member;
    }
    return null;
  }

  /**
   * 로그인 회원 id 조회
   *
   * @return 회원 id
   */
  public Long getMemberId() {
    if (SecurityContextHolder.getContext().getAuthentication().getPrincipal() instanceof MemberDetail member) {
      return member.getMemberId();
    }
    return null;
  }
}
