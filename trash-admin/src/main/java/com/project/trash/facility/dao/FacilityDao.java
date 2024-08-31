package com.project.trash.facility.dao;

import com.project.trash.member.request.MemberFacilityListRequest;
import com.project.trash.member.response.MemberFacilityListResponse;

import org.springframework.data.domain.Sort;
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
   * 등록한 시설물 목록 조회 총개수
   */
  public long count(Long memberSeq) {
    return template.count(Query.query(Criteria.where("memberSeq").is(memberSeq)), Long.class, "facility");
  }

  /**
   * 등록한 시설물 목록 조회
   */
  public List<MemberFacilityListResponse> select(MemberFacilityListRequest param) {
    Query query = Query.query(Criteria.where("memberSeq").is(param.getMemberSeq()));
    query.skip(param.getOffset());
    query.limit(param.getSize());
    query.with(Sort.by(Sort.Order.desc("createdAt")));

    return template.find(query, MemberFacilityListResponse.class, "facility");
  }
}
