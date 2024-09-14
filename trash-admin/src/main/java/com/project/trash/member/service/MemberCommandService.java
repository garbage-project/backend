package com.project.trash.member.service;

import com.project.trash.member.domain.Member;
import com.project.trash.member.repository.MemberRepository;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * 회원 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class MemberCommandService {

  private final MemberRepository memberRepository;
  private final MemberQueryService memberQueryService;

  /**
   * 회원 삭제
   */
  @Transactional
  public void delete(Long memberId) {
    Member member = memberQueryService.getOne(memberId);

    memberRepository.delete(member);
  }
}
