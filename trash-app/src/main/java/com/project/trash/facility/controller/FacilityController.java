package com.project.trash.facility.controller;

import com.project.trash.common.response.ListResponse;
import com.project.trash.common.response.SuccessResponse;
import com.project.trash.facility.controller.validation.FacilityValidator;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.service.FacilityCommandService;
import com.project.trash.facility.service.FacilityQueryService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;

/**
 * 시설물 API
 */
@RestController
@RequestMapping("/facilities")
@RequiredArgsConstructor
public class FacilityController {

  private final FacilityCommandService facilityCommandService;
  private final FacilityQueryService facilityQueryService;

  /**
   * 시설물 목록 조회
   */
  @GetMapping
  public ResponseEntity<?> getList(@ModelAttribute FacilityListRequest param) {
    FacilityValidator.validate(param);

    return ResponseEntity.ok(new ListResponse(facilityQueryService.getList(param)));
  }

  /**
   * 시설물 등록
   */
  @PostMapping
  public ResponseEntity<?> post(@RequestBody FacilityEntryRequest param) {
    FacilityValidator.validate(param);

    facilityCommandService.entry(param);
    return ResponseEntity.ok(new SuccessResponse());
  }
}
