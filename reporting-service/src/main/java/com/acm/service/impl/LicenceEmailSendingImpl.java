package com.acm.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.scheduling.annotation.Scheduled;

import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonFunctions;
import com.acm.scheduler.configuration.SendMailScheduler;
import com.acm.service.MailServiceEngine;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.MailDTO;

/**
 * The Class LicenceEmailSendingImpl.
 */
@Configuration
@Import({SendMailScheduler.class})
public class LicenceEmailSendingImpl {

	/** The mail service engine. */
	@Autowired
	MailServiceEngine mailServiceEngine;

	/** The parametrage client. */
	@Autowired
	ParametrageClient parametrageClient;
	/** The url serveur authentification. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/** The email en cc. */
	@Value("${com.acm.mail.cc}")
	private String emailEnCc;

	/** The user name. */
	@Value("${com.acm.mail.username}")
	private String userName;

	/**
	 * Seding email for licence reminder.
	 *
	 * @throws Exception the exception
	 */
	@Scheduled(cron = "#{@getCronValueForSendingMailLicence}") // => load value from DB
																// ACM_ENVIRONNEMENT
	public void sedingEmailForLicenceReminder() throws Exception {

		AcmEnvironnementDTO acmEnvironnementDTO = new AcmEnvironnementDTO();
		acmEnvironnementDTO.setKey("KEY_LICENCE");
		String token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		List<AcmEnvironnementDTO> acmEnvironnementDTOs =
				parametrageClient.findLikeKeyWithToken(acmEnvironnementDTO, token);
		MailDTO mailDTO = new MailDTO();
		mailDTO.setContent("your ACM licence expired in 30 Days");
		mailDTO.setFrom(userName);
		mailDTO.setTo(findOwnerFromKey(
				CommonFunctions.decryptWithPrivateKey(acmEnvironnementDTOs.get(0).getValue())));
		mailDTO.setSubject("ACM licence");
		mailDTO.setCc(emailEnCc);

		mailServiceEngine.prepareAndSend(mailDTO);

	}

	/**
	 * Find owner from key.
	 *
	 * @param key the key
	 * @return the string
	 */
	private String findOwnerFromKey(String key) {

		int index = 0;
		for (String s : key.split(",")) {
			index++;
			if (s.contains(CommonConstants.OWNER_LICENCE)) {
				break;
			}
		}

		return key.split(",")[index - 1].split(":")[1].trim();
	}

}
