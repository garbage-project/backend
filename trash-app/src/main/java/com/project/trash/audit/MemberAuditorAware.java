package com.project.trash.audit;

import com.project.trash.member.domain.MemberDetail;

import org.springframework.data.domain.AuditorAware;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.Optional;

/**
 * 등록자 정보
 */
@Component
public class MemberAuditorAware implements AuditorAware<Long> {

  @Override
  public Optional<Long> getCurrentAuditor() {
    final Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
    if (authentication == null) {
      return Optional.empty();
    }

    if (authentication.getPrincipal() instanceof MemberDetail memberDetail) {
      return Optional.of(memberDetail.getMemberId());
    }
    return Optional.empty();
  }
}
