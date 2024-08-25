package com.project.trash.member.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.member.domain.Member;
import com.project.trash.member.repository.MemberRepository;

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
   * 회원 조회
   */
  @Transactional(readOnly = true)
  public Member getOne(String socialId) {
    return memberRepository.findBySocialId(socialId).orElseThrow(() -> new ValidationException("member.not_found"));
  }
}
