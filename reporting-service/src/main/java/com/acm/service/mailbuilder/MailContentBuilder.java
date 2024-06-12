/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.mailbuilder;

import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.Context;

import com.acm.constants.common.CommonConstants;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.CustomerContactDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailIBLoanDTO;
import com.acm.utils.dtos.MailLoanDTO;

/**
 * {@link MailContentBuilder} class: where we build the content to be send in our mail based on
 * prepared Template.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Service
public class MailContentBuilder {

	/** The template engine. */
	@Autowired
	private TemplateEngine templateEngine;

	/** The url serveur IB. */
	@Value("${url.serveur.ib}")
	private String urlServeurIB;

	/** The url serveur ACM. */
	@Value("${url.serveur.acm}")
	private String urlServeurACM;

	/** The get system name. */
	@Autowired
	private String getSystemName;

	/** The get IP address. */
	@Autowired
	private String getIPAddress;

	/**
	 * Builds the mail body with given prams.
	 * 
	 * @author HaythemBenizid
	 * @param mailDTO the mail DTO
	 * @return the string
	 */
	public String build(MailDTO mailDTO) {

		Context context = new Context();
		context.setVariable("from", mailDTO.getFrom());
		context.setVariable("to", mailDTO.getTo());
		context.setVariable("subject", mailDTO.getSubject());
		context.setVariable("content", mailDTO.getContent());
		context.setVariable("host",
				"Mail send from : Host Name : " + getSystemName + " | Host IP : " + getIPAddress);

		return templateEngine.process("mailTemplate", context);
	}

	/**
	 * Builds the mail Submit after Financial Analysis body with given prams.
	 *
	 * @author AbdelkarimTurki
	 * @param mailLoanDTO the loan mail DTO
	 * @return the string
	 */
	public String buildSubmitEmail(MailLoanDTO mailLoanDTO) {

		Context context = new Context();
		context.setVariable("user", mailLoanDTO.getLoanDTO().getOwnerName());
		context.setVariable("loanAccountNumber", mailLoanDTO.getLoanDTO().getAccountNumber());
		context.setVariable("date", DateUtil.formatDate(mailLoanDTO.getLoanDTO().getCreationDate(),
				CommonConstants.PATTREN_DATE));
		context.setVariable("owner", mailLoanDTO.getUserConnected());
		context.setVariable("link", urlServeurACM);
		context.setVariable("linkLabel", CommonConstants.LINK_LABEL);
		context.setVariable("customerName",
				mailLoanDTO.getLoanDTO().getCustomerName().replace("|", " "));

		return templateEngine.process("submitForApprovalEmailTemplate.html", context);
	}

	/**
	 * Builds the mail check (L1,L2.L3.L4) Review email body with given prams.
	 *
	 * @author AbdelkarimTurki
	 * @param mailLoanDTO the loan mail DTO
	 * @return the string
	 */
	public String buildCheckReviewEmail(MailLoanDTO mailLoanDTO) {

		Context context = new Context();
		context.setVariable("user", mailLoanDTO.getLoanDTO().getOwnerName());
		context.setVariable("loanAccountNumber", mailLoanDTO.getLoanDTO().getAccountNumber());
		context.setVariable("date", DateUtil.formatDate(mailLoanDTO.getLoanDTO().getCreationDate(),
				CommonConstants.PATTREN_DATE));
		context.setVariable("owner", mailLoanDTO.getUserConnected());
		context.setVariable("link", urlServeurACM);
		context.setVariable("linkLabel", CommonConstants.LINK_LABEL);
		context.setVariable("customerName",
				mailLoanDTO.getLoanDTO().getCustomerName().replace("|", " "));

		return templateEngine.process("submitForReviewEmailTemplate.html", context);
	}

	/**
	 * Builds the mail Last Approval (L4) email body with given prams.
	 *
	 * @author AbdelkarimTurki
	 * @param mailLoanDTO the loan mail DTO
	 * @return the string
	 */
	public String buildCheckLastLevelApprovalEmail(MailLoanDTO mailLoanDTO) {

		Context context = new Context();
		context.setVariable("user", mailLoanDTO.getLoanDTO().getOwnerName());
		context.setVariable("loanAccountNumber", mailLoanDTO.getLoanDTO().getAccountNumber());
		context.setVariable("date", DateUtil.formatDate(mailLoanDTO.getLoanDTO().getCreationDate(),
				CommonConstants.PATTREN_DATE));
		context.setVariable("owner", mailLoanDTO.getUserConnected());
		context.setVariable("link", urlServeurACM);
		context.setVariable("linkLabel", CommonConstants.LINK_LABEL);
		context.setVariable("customerName",
				mailLoanDTO.getLoanDTO().getCustomerName().replace("|", " "));

		return templateEngine.process("checkLastLevelEmailTemplate.html", context);
	}

	/**
	 * Builds the mail check (L1,L2.L3.L4) Reject email body with given prams.
	 *
	 * @author AbdelkarimTurki
	 * @param mailLoanDTO the loan mail DTO
	 * @return the string
	 */
	public String buildCheckLevelsRejectEmail(MailLoanDTO mailLoanDTO) {

		Context context = new Context();
		context.setVariable("user", mailLoanDTO.getLoanDTO().getOwnerName());
		context.setVariable("loanAccountNumber", mailLoanDTO.getLoanDTO().getAccountNumber());
		context.setVariable("date", DateUtil.formatDate(mailLoanDTO.getLoanDTO().getCreationDate(),
				CommonConstants.PATTREN_DATE));
		context.setVariable("owner", mailLoanDTO.getUserConnected());
		context.setVariable("link", urlServeurACM);
		context.setVariable("linkLabel", CommonConstants.LINK_LABEL);
		context.setVariable("customerName",
				mailLoanDTO.getLoanDTO().getCustomerName().replace("|", " "));

		return templateEngine.process("checkLevelsRejectEmailTemplate.html", context);
	}

	/**
	 * Builds the mail Loan Rejected Client body with given prams.
	 *
	 * @author AbdelkarimTurki
	 * @param mailLoanDTO the loan mail DTO
	 * @return the string
	 */
	public String buildLoanRejectedClientEmail(MailLoanDTO mailLoanDTO) {

		Context context = new Context();
		context.setVariable("fromMfiName", CommonConstants.APP_NAME);
		context.setVariable("fromAddress", mailLoanDTO.getMailDTO().getFrom());
		context.setVariable("toName", mailLoanDTO.getLoanDTO().getCustomerName());
		context.setVariable("toAddress", mailLoanDTO.getMailDTO().getTo());
		context.setVariable("loanDate", DateUtil.formatDate(
				mailLoanDTO.getLoanDTO().getCreationDate(), CommonConstants.PATTREN_DATE));
		context.setVariable("reason", mailLoanDTO.getLoanDTO().getLoanReasonDescription());
		context.setVariable("note", mailLoanDTO.getLoanDTO().getNote());
		context.setVariable("loanOfficer", mailLoanDTO.getLoanDTO().getPortfolioDescription());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("loanRejectedClientEmailTemplate.html", context);
	}

	/**
	 * Builds the mail Inform Customer Ask For Review email body with given prams.
	 * 
	 * @author AbdelkarimTurki
	 * @param mailLoanDTO the loan mail DTO
	 * @return the string
	 */
	public String buildInformCustomerAskForReviewEmail(MailLoanDTO mailLoanDTO) {

		Context context = new Context();
		context.setVariable("user", mailLoanDTO.getLoanDTO().getOwnerName());
		context.setVariable("loanAccountNumber", mailLoanDTO.getLoanDTO().getAccountNumber());
		context.setVariable("date", DateUtil.formatDate(mailLoanDTO.getLoanDTO().getCreationDate(),
				CommonConstants.PATTREN_DATE));
		context.setVariable("owner", mailLoanDTO.getUserConnected());
		context.setVariable("link", urlServeurACM);
		context.setVariable("linkLabel", CommonConstants.LINK_LABEL);
		context.setVariable("customerName",
				mailLoanDTO.getLoanDTO().getCustomerName().replace("|", " "));

		return templateEngine.process("clientAskedForReviewEmailTemplate.html", context);
	}

	/**
	 * Builds the mail Inform Customer approved email body with given prams.
	 * 
	 * @author Salmen Fatnassi
	 * @param mailLoanDTO the loan mail DTO
	 * @return the string
	 */
	public String buildInformClientLoanAssignedEmail(MailLoanDTO mailLoanDTO) {

		Context context = new Context();
		context.setVariable("user", mailLoanDTO.getLoanDTO().getOwnerName());
		context.setVariable("loanAccountNumber", mailLoanDTO.getLoanDTO().getAccountNumber());
		context.setVariable("date", DateUtil.formatDate(mailLoanDTO.getLoanDTO().getCreationDate(),
				CommonConstants.PATTREN_DATE));
		context.setVariable("loanOfficer", mailLoanDTO.getLoanDTO().getPortfolioDescription());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));
		context.setVariable("content", mailLoanDTO.getMailDTO().getContent());

		return templateEngine.process("clientLoanAssignedEmailTemplate.html", context);
	}

	/**
	 * Builds the mail Inform Customer approved email body with given prams.
	 * 
	 * @author AbdelkarimTurki
	 * @param mailLoanDTO the loan mail DTO
	 * @return the string
	 */
	public String buildInformCustomerApprovedEmail(MailLoanDTO mailLoanDTO) {

		Context context = new Context();
		context.setVariable("user", mailLoanDTO.getLoanDTO().getOwnerName());
		context.setVariable("loanAccountNumber", mailLoanDTO.getLoanDTO().getAccountNumber());
		context.setVariable("date", DateUtil.formatDate(mailLoanDTO.getLoanDTO().getCreationDate(),
				CommonConstants.PATTREN_DATE));
		context.setVariable("loanOfficer", mailLoanDTO.getLoanDTO().getPortfolioDescription());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("loanAcceptedEmailTemplate.html", context);
	}

	/**
	 * Builds the mail Inform Customer Decline email body with given prams.
	 * 
	 * @author AbdelkarimTurki
	 * @param mailLoanDTO the loan mail DTO
	 * @return the string
	 */
	public String buildInformCustomerDeclineEmail(MailLoanDTO mailLoanDTO) {

		Context context = new Context();
		context.setVariable("date", DateUtil.formatDate(mailLoanDTO.getLoanDTO().getCreationDate(),
				CommonConstants.PATTREN_DATE));
		context.setVariable("loanOfficer", mailLoanDTO.getLoanDTO().getPortfolioDescription());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("loanDeclinedClientEmailTemplate.html", context);
	}

	/**
	 * Builds the mail Inform Customer Accept email body with given prams.
	 *
	 * @author HaythemBenizid
	 * @param mailCustomerDTO the mail customer DTO
	 * @return the string
	 */
	public String buildInformCustomerAcceptEmail(MailCustomerDTO mailCustomerDTO) {

		Context context = new Context();
		context.setVariable("URL_IB", urlServeurIB);
		context.setVariable("login", mailCustomerDTO.getLogin());
		context.setVariable("pwd", mailCustomerDTO.getPwd());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("ibLoanAcceptedEmailTemplate.html", context);
	}

	/**
	 * Builds the new customer email.
	 *
	 * @author ManelLamloum
	 * @param mailCustomerDTO the mail customer DTO
	 * @return the string
	 */
	public String buildNewCustomerEmail(MailCustomerDTO mailCustomerDTO) {

		Context context = new Context();
		context.setVariable("URL_IB", urlServeurIB);
		context.setVariable("login", mailCustomerDTO.getLogin());
		context.setVariable("pwd", mailCustomerDTO.getPwd());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("buildNewCustomerTemplate.html", context);
	}

	/**
	 * Builds the contact email body with given prams.
	 *
	 * @author Salmen Fatnassi
	 * @param customerContactDTO the customer contact DTO
	 * @return the string
	 */
	public String buildContactEmail(CustomerContactDTO customerContactDTO) {

		Context context = new Context();
		context.setVariable("from", customerContactDTO.getFrom());
		context.setVariable("to", customerContactDTO.getTo());
		context.setVariable("subject", customerContactDTO.getSubject());
		context.setVariable("content", customerContactDTO.getContent());

		return templateEngine.process("mailContactTemplate.html", context);
	}

	/**
	 * Builds the mail Inform Customer Accept email body with given prams.
	 *
	 * @author Salmen Fatnassi
	 * @param mailIBLoanDTO the mail ibloan DTO
	 * @return the string
	 */
	public String buildLoanRejectedClientEmailForMailCustomer(MailIBLoanDTO mailIBLoanDTO) {

		Context context = new Context();
		context.setVariable("fromMfiName", CommonConstants.APP_NAME);
		context.setVariable("fromAddress", mailIBLoanDTO.getMailDTO().getFrom());
		context.setVariable("toName", mailIBLoanDTO.getIbLoanDTO().getCustomerName());
		context.setVariable("toAddress", mailIBLoanDTO.getMailDTO().getTo());
		context.setVariable("loanDate", DateUtil.formatDate(
				mailIBLoanDTO.getIbLoanDTO().getDateInsertion(), CommonConstants.PATTREN_DATE));
		context.setVariable("reason", "");
		context.setVariable("note", mailIBLoanDTO.getIbLoanDTO().getNote());
		context.setVariable("loanOfficer", mailIBLoanDTO.getIbLoanDTO().getPortfolioDescription());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("loanRejectedClientEmailTemplate.html", context);
	}

	/**
	 * Builds the mail Inform User reset email body with given prams.
	 *
	 * @author MoezMhiri
	 * @param mailCustomerDTO the mail customer DTO
	 * @return the string
	 */
	public String buildInformCustomerResetPwd(MailCustomerDTO mailCustomerDTO) {

		Context context = new Context();
		context.setVariable("URL_ACM", urlServeurACM);
		context.setVariable("login", mailCustomerDTO.getLogin());
		context.setVariable("pwd", mailCustomerDTO.getPwd());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("resetPwdTemplate.html", context);

	}

	/**
	 * Builds the mail Inform User resend email body with given prams.
	 *
	 * @author MoezMhiri
	 * @param mailCustomerDTO the mail customer DTO
	 * @return the string
	 */
	public String buildInformUserResendLogin(MailCustomerDTO mailCustomerDTO) {

		Context context = new Context();
		context.setVariable("URL_IB", urlServeurIB);
		context.setVariable("login", mailCustomerDTO.getLogin());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("resendLoginTemplate.html", context);
	}

	/**
	 * Builds the add user email.
	 *
	 * @author ManelLamloum
	 * @param mailCustomerDTO the mail customer DTO
	 * @return the string
	 */
	public String buildAddUserEmail(MailCustomerDTO mailCustomerDTO) {

		Context context = new Context();
		context.setVariable("login", mailCustomerDTO.getLogin());
		context.setVariable("pwd", mailCustomerDTO.getPwd());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("addUserEmailTemplate.html", context);
	}

	/**
	 * Builds the inform user assigned meza card email.
	 * 
	 * @author idridi
	 * @param mailCustomerDTO the mail customer DTO
	 * @return the string
	 */
	public String buildInformUserAssignedMezaCardEmail(MailCustomerDTO mailCustomerDTO) {

		Context context = new Context();
		context.setVariable("userReceiver", mailCustomerDTO.getUserDTO().getSimpleName());
		context.setVariable("sendDate",
				DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE));

		return templateEngine.process("assignedMezaCardMail.html", context);
	}
}
