package com.project.trash.admin.repository;

import com.project.trash.admin.domain.Admin;

import org.springframework.data.jpa.repository.JpaRepository;

public interface AdminRepository extends JpaRepository<Admin, String> {
}
