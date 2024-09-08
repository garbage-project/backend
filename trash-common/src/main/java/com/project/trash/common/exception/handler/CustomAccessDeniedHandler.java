package com.project.trash.common.exception.handler;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.project.trash.common.domain.resultcode.RequestResultCode;
import com.project.trash.common.response.ErrorResponse;

import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.stereotype.Component;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;

/**
 * 인가 예외 핸들러
 */
@Slf4j
@Component
public class CustomAccessDeniedHandler implements AccessDeniedHandler {

  @Override
  public void handle(HttpServletRequest request, HttpServletResponse response,
      AccessDeniedException accessDeniedException) {
    ErrorResponse errorResponse = new ErrorResponse(RequestResultCode.FORBIDDEN);
    response.setContentType("application/json;charset=UTF-8");
    response.setStatus(HttpStatus.FORBIDDEN.value());

    try {
      response.getWriter().print(new ObjectMapper().writeValueAsString(errorResponse));
    } catch (Exception e) {
      log.error(e.getMessage());
    }
  }
}
