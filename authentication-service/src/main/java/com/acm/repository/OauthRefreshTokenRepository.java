/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import com.acm.model.OauthRefreshToken;

/**
 * {@link OauthRefreshTokenRepository} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Repository
public interface OauthRefreshTokenRepository extends JpaRepository<OauthRefreshToken, String>,
		CrudRepository<OauthRefreshToken, String> {

}
