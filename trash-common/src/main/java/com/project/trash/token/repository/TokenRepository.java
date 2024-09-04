package com.project.trash.token.repository;

import com.project.trash.token.domain.Token;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface TokenRepository extends JpaRepository<Token, String> {
  
  Optional<Token> findByMemberId(String memberId);
}
