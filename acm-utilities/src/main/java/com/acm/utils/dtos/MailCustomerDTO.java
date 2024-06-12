/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.acm.utils.enums.MailBuilderMethod;

/**
 * {@link MailCustomerDTO } class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class MailCustomerDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2677212261128913278L;

	/** The customer DTO. */
	private CustomerDTO customerDTO;

	/** The user DTO. */
	private UserDTO userDTO;

	/** The login. */
	private String login;

	/** The pwd. */
	private String pwd;

	/** The mail DTO. */
	private MailDTO mailDTO;

	/** The mail builder method. */
	private MailBuilderMethod mailBuilderMethod;

	/**
	 * Instantiates a new mail customer DTO.
	 */
	public MailCustomerDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new mail customer DTO.
	 *
	 * @param customerDTO the customer DTO
	 * @param mailDTO the mail DTO
	 * @param mailBuilderMethod the mail builder method
	 */
	public MailCustomerDTO(CustomerDTO customerDTO, MailDTO mailDTO,
			MailBuilderMethod mailBuilderMethod) {

		this.customerDTO = customerDTO;
		this.mailDTO = mailDTO;
		this.mailBuilderMethod = mailBuilderMethod;
	}

	/**
	 * Instantiates a new mail customer DTO.
	 *
	 * @param customerDTO the customer DTO
	 * @param login the login
	 * @param pwd the pwd
	 * @param mailDTO the mail DTO
	 * @param mailBuilderMethod the mail builder method
	 */
	public MailCustomerDTO(CustomerDTO customerDTO, String login, String pwd, MailDTO mailDTO,
			MailBuilderMethod mailBuilderMethod) {

		this.customerDTO = customerDTO;
		this.login = login;
		this.pwd = pwd;
		this.mailDTO = mailDTO;
		this.mailBuilderMethod = mailBuilderMethod;
	}

	/**
	 * Instantiates a new mail user DTO.
	 *
	 * @param userDTO the user DTO
	 * @param login the login
	 * @param pwd the pwd
	 * @param mailDTO the mail DTO
	 * @param mailBuilderMethod the mail builder method
	 */
	public MailCustomerDTO(UserDTO userDTO, String login, String pwd, MailDTO mailDTO,
			MailBuilderMethod mailBuilderMethod) {

		this.setUserDTO(userDTO);
		this.login = login;
		this.pwd = pwd;
		this.mailDTO = mailDTO;
		this.mailBuilderMethod = mailBuilderMethod;
	}

	/**
	 * Instantiates a new mail customer DTO.
	 *
	 * @param userDTO the user DTO
	 * @param login the login
	 * @param mailDTO the mail DTO
	 * @param mailBuilderMethod the mail builder method
	 */
	public MailCustomerDTO(UserDTO userDTO, String login, MailDTO mailDTO,
			MailBuilderMethod mailBuilderMethod) {

		this.setUserDTO(userDTO);
		this.login = login;
		this.mailDTO = mailDTO;
		this.mailBuilderMethod = mailBuilderMethod;
	}

	/**
	 * Gets the mail DTO.
	 *
	 * @return the mail DTO
	 */
	public MailDTO getMailDTO() {

		return mailDTO;
	}

	/**
	 * Sets the loan.
	 * 
	 * @param mailDTO the mailDTO to set
	 */
	public void setMailDTO(MailDTO mailDTO) {

		this.mailDTO = mailDTO;
	}

	/**
	 * Gets the mail builder method.
	 *
	 * @return the mailBuilderMethod
	 */
	public MailBuilderMethod getMailBuilderMethod() {

		return mailBuilderMethod;
	}

	/**
	 * Sets the mail builder method.
	 *
	 * @param mailBuilderMethod the mailBuilderMethod to set
	 */
	public void setMailBuilderMethod(MailBuilderMethod mailBuilderMethod) {

		this.mailBuilderMethod = mailBuilderMethod;
	}

	/**
	 * Gets the customer DTO.
	 *
	 * @return the customerDTO
	 */
	public CustomerDTO getCustomerDTO() {

		return customerDTO;
	}

	/**
	 * Sets the customer DTO.
	 *
	 * @param customerDTO the customerDTO to set
	 */
	public void setCustomerDTO(CustomerDTO customerDTO) {

		this.customerDTO = customerDTO;
	}

	/**
	 * Gets the login.
	 *
	 * @return the login
	 */
	public String getLogin() {

		return login;
	}

	/**
	 * Sets the login.
	 *
	 * @param login the login to set
	 */
	public void setLogin(String login) {

		this.login = login;
	}

	/**
	 * Gets the pwd.
	 *
	 * @return the pwd
	 */
	public String getPwd() {

		return pwd;
	}

	/**
	 * Sets the pwd.
	 *
	 * @param pwd the pwd to set
	 */
	public void setPwd(String pwd) {

		this.pwd = pwd;
	}

	/**
	 * Gets the user DTO.
	 *
	 * @return the userDTO
	 */
	public UserDTO getUserDTO() {

		return userDTO;
	}

	/**
	 * Sets the user DTO.
	 *
	 * @param userDTO the userDTO to set
	 */
	public void setUserDTO(UserDTO userDTO) {

		this.userDTO = userDTO;
	}

}
