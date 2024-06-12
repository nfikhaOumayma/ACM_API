/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.oauth2server.service;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import com.acm.utils.models.Groupe;

/**
 * {@link GrantedAuthorityCreater} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Component
public class GrantedAuthorityCreater implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 5165593617995658164L;

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(GrantedAuthorityCreater.class);

	/**
	 * Gets the granted authority creater.
	 *
	 * @return the granted authority creater
	 */
	@Bean
	public GrantedAuthorityCreater getGrantedAuthorityCreater() {

		return new GrantedAuthorityCreater();
	}

	/**
	 * Find authoritys.
	 *
	 * @param roles the roles
	 * @return the list
	 */
	public List<SimpleGrantedAuthority> findAuthoritys(Set<Groupe> roles) {

		logger.debug("convert Authority");
		List<SimpleGrantedAuthority> authList = new ArrayList<>();
		roles.forEach(role -> authList
				.add(new SimpleGrantedAuthority("ROLE_" + role.getCode().toUpperCase())));
		return Collections.unmodifiableList(authList);
	}
}
