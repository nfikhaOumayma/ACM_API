/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.acm.client.ReportingClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonFunctions;
import com.acm.service.AcmDocumentsGedService;
import com.acm.utils.dtos.AcmDocumentsGedDTO;
import com.acm.utils.dtos.MailDTO;

/**
 * {@link CronBatchService} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Component
public class CronBatchService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CronBatchService.class);

	/** The acm documents ged service. */
	@Autowired
	private AcmDocumentsGedService acmDocumentsGedService;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The token. */
	private String token = "NOT";

	/** The url serveur authentification. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/**
	 * Send Notification.
	 * 
	 * @author HaythemBenizid
	 */
	@Scheduled(cron = "${cron.expression.ged.batch}") // => load value from .properties file
	public void sendNotif() {

		try {
			if ("NOT".equals(token)) {
				token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
			}
			// check if there is a document in AcmDocumentsGed Table in ACM-DB
			List<AcmDocumentsGedDTO> acmDocumentsGedDTOs =
					acmDocumentsGedService.find(new AcmDocumentsGedDTO());
			Integer count = acmDocumentsGedDTOs.size();
			// send notif mail if documents founded
			if (count != 0) {
				mailSenderClient
						.sendMail(
								new MailDTO(CommonConstants.NO_REPLAY_EMAIL, defaultACMReceiverMail,
										"Processing DMS health Check",
										"Note that there was a problem in your DMS and that ["
												+ count + "] documents were stored in ACM-DB"),
								token);
				logger.debug("Sending Email for Processing DMS health Check :: DONE");
			}
		}
		catch (Exception e) {
			logger.error("### Failed to run Check DMS health Job ###");
		}
	}
}
