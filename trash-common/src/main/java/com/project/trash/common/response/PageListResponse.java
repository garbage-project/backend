package com.project.trash.common.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.project.trash.common.domain.Pagination;
import com.project.trash.common.request.PageRequest;

import java.util.Collection;

import lombok.Getter;

/**
 * 페이지 & 목록 조회 공통 응답 형식
 */
@Getter
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({"status", "message", "page", "list"})
public class PageListResponse extends SuccessResponse {

  private Pagination page;
  private final Collection<?> list;

  public PageListResponse(PageRequest param, Collection<?> list, Long totalItems) {
    if (totalItems > 0) {
      this.page = new Pagination(param, totalItems);
    }
    this.list = list;
  }
}
