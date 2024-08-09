package com.project.trash.member.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.member.domain.Member;
import com.project.trash.member.domain.MemberDetail;
import com.project.trash.member.repository.MemberRepository;
import com.project.trash.member.response.MemberDetailResponse;
import com.project.trash.utils.MemberUtils;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * 회원 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class MemberQueryService {

  private final MemberRepository memberRepository;

  /**
   * 로그인 회원 상세 조회
   */
  public MemberDetailResponse getDetail() {
    MemberDetail member = MemberUtils.getMember();
    return new MemberDetailResponse(member.getName(), member.getSocialId());
  }

  @Transactional(readOnly = true)
  public Member getOne(String socialId) {
    return memberRepository.findBySocialId(socialId).orElseThrow(() -> new ValidationException("member.not_found"));
  }
}
