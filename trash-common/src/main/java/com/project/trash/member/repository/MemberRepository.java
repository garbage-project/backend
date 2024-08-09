package com.project.trash.member.repository;

import com.project.trash.member.domain.Member;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface MemberRepository extends JpaRepository<Member, Long> {

  boolean existsBySocialId(String socialId);

  Optional<Member> findBySocialId(String socialId);
}
