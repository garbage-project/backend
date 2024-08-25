package com.project.trash.common.domain;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.project.trash.common.request.PageRequest;

import org.apache.commons.lang3.math.NumberUtils;

import lombok.Getter;

@Getter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Pagination {

  private Long totalItems;
  private Integer totalPages;
  private Integer currentPage;
  private Boolean hasNext;
  private Boolean hasPrevious;
  private Boolean isFirst;
  private Boolean isLast;

  public Pagination(PageRequest param, long totalItems) {
    if (totalItems > 0) {
      this.totalItems = totalItems;
      calculation(param);
    }
  }

  private void calculation(PageRequest param) {
    int size = param.getSize();
    int page = param.getPage();

    this.currentPage = page;

    this.hasNext = NumberUtils.compare(totalItems - ((long) size * page), 0) > 0;
    this.hasPrevious = NumberUtils.compare(page, 1) > 0;

    this.isFirst = NumberUtils.compare(page, 1) == 0;
    this.isLast = NumberUtils.compare(totalItems, (long) size * page) == 0 ||
        NumberUtils.compare(totalItems, (long) size * page) < 0;

    this.totalPages = (int) Math.ceil((totalItems * 1.0d) / (size * 1.d));
  }
}
