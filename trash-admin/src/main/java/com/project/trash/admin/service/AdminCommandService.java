package com.project.trash.admin.service;

import com.project.trash.admin.domain.Admin;
import com.project.trash.admin.request.AdminModifyRequest;
import com.project.trash.utils.AdminUtils;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * 관리자 수정 서비스
 */
@Service
@RequiredArgsConstructor
public class AdminCommandService {

  private final AdminQueryService adminQueryService;

  /**
   * 관리자 정보 수정
   */
  @Transactional
  public void modify(AdminModifyRequest param) {
    Admin admin = adminQueryService.getOne(AdminUtils.getId());

    admin.update(param.getPassword());
  }
}
