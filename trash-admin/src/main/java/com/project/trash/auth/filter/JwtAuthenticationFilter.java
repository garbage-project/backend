package com.project.trash.auth.filter;

import com.project.trash.admin.domain.AdminDetail;
import com.project.trash.admin.service.AdminQueryService;
import com.project.trash.auth.service.JwtService;
import com.project.trash.common.utils.CookieUtils;

import org.apache.commons.lang3.StringUtils;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;

/**
 * JwtAuthenticationFilter
 */
@RequiredArgsConstructor
public class JwtAuthenticationFilter extends OncePerRequestFilter {

  private final JwtService jwtService;
  private final AdminQueryService adminQueryService;

  @Override
  protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
      throws ServletException, IOException {
    String accessToken = CookieUtils.getCookie(request, "accessToken");

    if (StringUtils.isBlank(accessToken)) {
      filterChain.doFilter(request, response);
      return;
    }

    String adminId = jwtService.extractUsername(accessToken);
    if (StringUtils.isNotBlank(adminId)) {
      AdminDetail adminDetail = new AdminDetail(adminQueryService.getOne(adminId));

      // 유효성 체크
      if (jwtService.isTokenValid(accessToken, adminDetail)) {
        Authentication authentication =
            new UsernamePasswordAuthenticationToken(adminDetail, accessToken, adminDetail.getAuthorities());
        SecurityContextHolder.getContext().setAuthentication(authentication);
      }
    }

    filterChain.doFilter(request, response);
  }
}
