package com.project.trash.facility.request;

import com.project.trash.common.request.PageRequest;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FacilityReviewListRequest extends PageRequest {

  @Parameter(description = "시설물 ID", example = "1", required = true)
  private Long facilityId;
}
