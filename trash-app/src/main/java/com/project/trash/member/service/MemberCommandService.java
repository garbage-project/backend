package com.project.trash.member.service;

import com.project.trash.member.repository.MemberRepository;

import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;

/**
 * 회원 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class MemberCommandService {

  private final MemberRepository memberRepository;
}
