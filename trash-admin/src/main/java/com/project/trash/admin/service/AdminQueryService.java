package com.project.trash.admin.service;

import com.project.trash.admin.domain.Admin;
import com.project.trash.admin.repository.AdminRepository;
import com.project.trash.common.exception.ValidationException;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * 관리자 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class AdminQueryService {

  private final AdminRepository adminRepository;

  @Transactional(readOnly = true)
  public Admin getOne(String id) {
    return adminRepository.findById(id).orElseThrow(() -> new ValidationException("admin.not_found"));
  }
}
