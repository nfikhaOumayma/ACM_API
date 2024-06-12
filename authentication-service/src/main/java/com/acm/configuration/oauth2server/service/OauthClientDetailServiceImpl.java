/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.oauth2server.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Primary;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.oauth2.provider.ClientDetails;
import org.springframework.security.oauth2.provider.ClientDetailsService;
import org.springframework.security.oauth2.provider.client.BaseClientDetails;
import org.springframework.stereotype.Service;

import com.acm.model.OauthClientDetail;
import com.acm.repository.OauthClientDetailRepository;

/**
 * {@link OauthClientDetailServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Primary
@Service
public class OauthClientDetailServiceImpl implements ClientDetailsService {

	/** Default Mode is INFO. */
	private static final Logger logger =
			LoggerFactory.getLogger(OauthClientDetailServiceImpl.class);

	/** The oauth client detail repository. */
	@Autowired
	private OauthClientDetailRepository oauthClientDetailRepository;

	/*
	 * (non-Javadoc)
	 * @see
	 * org.springframework.security.oauth2.provider.ClientDetailsService#loadClientByClientId(java.
	 * lang.String)
	 */
	@Override
	public ClientDetails loadClientByClientId(String clientId) {

		final OauthClientDetail oauthClientDetail =
				oauthClientDetailRepository.findById(clientId).orElse(null);
		if (oauthClientDetail != null) {
			BaseClientDetails clientDetails = new BaseClientDetails();
			clientDetails.setClientId(oauthClientDetail.getClientId());
			clientDetails.setClientSecret(oauthClientDetail.getClientSecret());

			clientDetails.setAccessTokenValiditySeconds(oauthClientDetail.getAccessTokenValidity());
			clientDetails
					.setRefreshTokenValiditySeconds(oauthClientDetail.getRefreshTokenValidity());

			clientDetails.setAuthorizedGrantTypes(new HashSet<>(
					Arrays.asList(oauthClientDetail.getAuthorizedGrantTypes().split(",", -1))));
			clientDetails.setResourceIds(new HashSet<>(
					Arrays.asList(oauthClientDetail.getResourceIds().split(",", -1))));
			clientDetails.setScope(
					new HashSet<>(Arrays.asList(oauthClientDetail.getScope().split(",", -1))));

			List<SimpleGrantedAuthority> authList = new ArrayList<>();
			authList.add(new SimpleGrantedAuthority(oauthClientDetail.getAuthorities()));
			clientDetails.setAuthorities(Collections.unmodifiableList(authList));
			return clientDetails;
		}
		else {
			logger.error("failed to load Client details by ID {}", clientId);
			return null;
		}
	}
}
