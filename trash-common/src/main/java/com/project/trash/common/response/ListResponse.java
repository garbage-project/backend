package com.project.trash.common.response;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.Collection;

import lombok.Getter;

/**
 * 목록 조회 공통 응답 형식
 */
@Getter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ListResponse<T> extends SuccessResponse {

  private final Collection<T> list;
  private final int count;

  public ListResponse(Collection<T> list) {
    this.list = list;
    this.count = list == null ? 0 : list.size();
  }
}
