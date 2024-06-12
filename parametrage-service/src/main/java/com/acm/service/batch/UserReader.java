/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemReader;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.acm.client.TransversClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.User;
import com.acm.utils.validation.ACMValidationUtils;

import feign.FeignException;

/**
 * {@link UserReader} class.
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
public class UserReader implements ItemReader<User> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(UserReader.class);

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The users abacus. */
	private List<UserDTO> usersAbacus = new ArrayList<>();

	/** The count. */
	private int count = 0;

	/** The token. */
	private String token = "NOT";

	/** The url serveur authentification. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemReader#read()
	 */
	@Override
	public User read() {

		logger.debug("### init reader process");
		if ("NOT".equals(token)) {
			token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		}
		// loading list user from ABACUS DB
		try {
			if (ACMValidationUtils.isNullOrEmpty(usersAbacus)) {
				usersAbacus = transversClient.findUsers(token, Long.valueOf("0"));
			}
		}
		catch (FeignException e) {
			logger.error("Failed to get list");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// re-generate token if expired
			token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		}
		// reading founded list if not empty
		if (count < usersAbacus.size()) {
			User user = new User(usersAbacus.get(count).getLogin(),
					usersAbacus.get(count).getAccountPortfolioId(),
					usersAbacus.get(count).getUserExternId(),
					usersAbacus.get(count).getUserProfilId(), usersAbacus.get(count).getBranchID(),
					usersAbacus.get(count).getBranchName(),
					usersAbacus.get(count).getBranchDescription(), token);
			user.setEnabled(usersAbacus.get(count).getActive());
			count++;
			return user;
		}
		else {
			count = 0;
		}
		return null;
	}
}
