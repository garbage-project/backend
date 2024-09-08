package com.project.trash.admin.service;

import com.project.trash.admin.domain.Admin;
import com.project.trash.admin.repository.AdminRepository;
import com.project.trash.common.exception.ValidationException;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.AdminResultCode.ADMIN_NOT_FOUND;

/**
 * 관리자 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class AdminQueryService {

  private final AdminRepository adminRepository;

  @Transactional(readOnly = true)
  public Admin getOne(String id) {
    return adminRepository.findById(id).orElseThrow(() -> new ValidationException(ADMIN_NOT_FOUND));
  }
}
