package com.project.trash.admin.service;

import com.project.trash.admin.domain.Admin;
import com.project.trash.admin.repository.AdminRepository;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.token.domain.Token;
import com.project.trash.token.repository.TokenRepository;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.AdminResultCode.ADMIN_NOT_FOUND;

@Service
@RequiredArgsConstructor
public class AdminQueryService {

  private final AdminRepository adminRepository;
  private final TokenRepository tokenRepository;

  @Transactional(readOnly = true)
  public Admin getOne(String id) {
    return adminRepository.findById(id).orElseThrow(() -> new ValidationException(ADMIN_NOT_FOUND));
  }

  @Transactional(readOnly = true)
  public Optional<Token> getToken(String id) {
    return tokenRepository.findByMemberId(id);
  }
}
