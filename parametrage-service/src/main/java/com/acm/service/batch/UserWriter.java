/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.ReportingClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.User;
import com.acm.utils.validation.ACMValidationUtils;

import feign.FeignException;

/**
 * {@link UserWriter} class.
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
public class UserWriter implements ItemWriter<User> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(UserWriter.class);

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends User> users) throws Exception {

		logger.debug("### UserWriter : list size = {}", users.size());
		if (!ACMValidationUtils.isNullOrEmpty(users)) {
			// saving data in DB
			saveUsers(users);
		}
		logger.debug("### UserWriter :: DONE");
	}

	/**
	 * Save users.
	 * 
	 * @author HaythemBenizid
	 * @param users the users
	 */
	private void saveUsers(List<? extends User> users) {

		List<User> newEntrys = new ArrayList<>();
		for (User user : users) {
			try {
				// insert imported user into ACM DB
				UserDTO userDTO = new UserDTO(user.getUsername(), user.getAccountPortfolioId(),
						user.getUserExternId(), user.getUserProfilId(), user.getBranchID(),
						user.getBranchName(), user.getBranchDescription());
				userDTO.setActive(user.getEnabled());
				// split String to array of String
				String[] elements = userDTO.getLogin().split("\\.");
				// setting data
				if (!ACMValidationUtils.isNullOrEmpty(elements) && elements.length > 1) {
					userDTO.setPrenom(elements[0].trim());
					userDTO.setNom(elements[1].trim());
				}
				else {
					userDTO.setPrenom(userDTO.getLogin());
				}
				// TODO champs mail a voir
				userDTO.setEmail(userDTO.getLogin() + "@talys.com");
				UserDTO newUserDTO = userClient.createForBatch(userDTO, user.getToken());
				logger.debug(
						"User with Login = [{}] / id_user_Extern = [{}] was successfully added in ACM DB",
						newUserDTO.getLogin(), newUserDTO.getUserExternId());

				if (Boolean.TRUE.equals(newUserDTO.getNewEntry())) {
					newEntrys.add(user);
				}
			}
			catch (FeignException e) {
				logger.error("Failed to save the user in DB");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
		}
		logger.debug("saving new {} users in ACM-DB :: DONE", newEntrys.size());
		// notification par mail
		sendMail(newEntrys);
	}

	/**
	 * Send mail.
	 * 
	 * @author HaythemBenizid
	 * @param users the users
	 */
	private void sendMail(List<? extends User> users) {

		if (!ACMValidationUtils.isNullOrEmpty(users)) {
			try {
				mailSenderClient.sendMail(
						new MailDTO(CommonConstants.NO_REPLAY_EMAIL, defaultACMReceiverMail,
								"Processing USERS DATA from ABACUS-DB :: DONE",
								"Processing : [" + users.size() + "] Users successfully."),
						users.get(0).getToken());
			}
			catch (FeignException e) {
				logger.error("Failed to send Mail");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
			logger.debug("Sending Email Notification :: DONE");
		}
	}
}
