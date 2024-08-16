package com.project.trash.facility.dao;

import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.response.FacilityListResponse;

import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

import lombok.RequiredArgsConstructor;

/**
 * 시설물 조회 DAO
 */
@Repository
@RequiredArgsConstructor
public class FacilityDao {

  private final MongoTemplate template;

  /**
   * 시설물 목록 조회
   */
  public List<FacilityListResponse> select(FacilityListRequest param) {
    Query query = new Query();
    query.addCriteria(Criteria.where("approvalStatus").is(FacilityApprovalStatus.APPROVE.getCode()));
    if (param.getType() != null) {
      query.addCriteria(Criteria.where("type").in(param.getType()));
    }

    return template.find(query, FacilityListResponse.class, "facility");
  }
}
