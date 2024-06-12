/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.ReportingClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.type.CustomerContactException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.CustomerContactRepository;
import com.acm.service.CustomerContactService;
import com.acm.service.CustomerService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CustomerContactDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.NotificationStatut;
import com.acm.utils.models.CustomerContact;
import com.acm.utils.models.QCustomerContact;
import com.acm.utils.string.StringUtils;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

import feign.FeignException;

/**
 * {@link CustomerContactServiceImpl} CustomerContactServiceImpl.
 *
 * @author AbdelkarimTurki
 * @since 0.17.0
 */
@Service
public class CustomerContactServiceImpl implements CustomerContactService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CustomerContactServiceImpl.class);

	/** The customerContact repository. */
	@Autowired
	private CustomerContactRepository customerContactRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerContactService#find(java.lang.Long)
	 */
	@Override
	public CustomerContactDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find CustomerContactDTO by ID : {}", id);
		CustomerContact customerContact = customerContactRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(customerContact)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					CustomerContactDTO.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ CustomerContactDTO.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(customerContact, CustomerContactDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerContactService#find(com.acm.utils.dtos.CustomerContactDTO)
	 */
	@Override
	public List<CustomerContactDTO> find(CustomerContactDTO customerContactDTO) {

		Preconditions.checkNotNull(customerContactDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init QCustomerContact
		QCustomerContact qCustomerContact = QCustomerContact.customerContact;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qCustomerContact.enabled.eq(Boolean.TRUE));

		// find connected user
		// find by UserName
		predicate.and(qCustomerContact.userName
				.eq(CommonFunctions.getConnectedUser(logger).getLogin()));

		// find by CustomerContact Id
		if (!ACMValidationUtils.isNullOrEmpty(customerContactDTO.getId())) {
			predicate.and(qCustomerContact.id.eq(customerContactDTO.getId()));
		}

		// find by Customer Id
		if (!ACMValidationUtils.isNullOrEmpty(customerContactDTO.getCustomerId())) {
			predicate.and(qCustomerContact.customerId.eq(customerContactDTO.getCustomerId()));
		}

		// find by From
		if (!ACMValidationUtils.isNullOrEmpty(customerContactDTO.getSentCustomer())) {
			predicate.and(qCustomerContact.sentCustomer.eq(customerContactDTO.getSentCustomer()));
		}

		// find by linkreply
		if (!ACMValidationUtils.isNullOrEmpty(customerContactDTO.getLinkReplay())) {
			predicate.and(qCustomerContact.linkReplay.eq(customerContactDTO.getLinkReplay()));
		}

		Iterable<CustomerContact> iterable = customerContactRepository.findAll(predicate,
				Sort.by(Direction.DESC, "dateInsertion"));
		List<CustomerContact> customerContacts = new ArrayList<>();
		iterable.forEach(customerContacts::add);
		logger.info("{} : CustomerContact was founded", customerContacts.size());

		List<CustomerContactDTO> customerContactsDTOs = new ArrayList<>();
		customerContacts.forEach(customerContact -> customerContactsDTOs
				.add(mapper.map(customerContact, CustomerContactDTO.class)));
		return customerContactsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerContactService#save(com.acm.utils.dtos.CustomerContactDTO)
	 */
	@Override
	public CustomerContactDTO save(CustomerContactDTO customerContactDTO) {

		Preconditions.checkNotNull(customerContactDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		customerContactDTO.setFrom(CommonConstants.NO_REPLAY_EMAIL);
		customerContactDTO.setTo(!ACMValidationUtils.isNullOrEmpty(customerContactDTO.getEmail())
				&& Boolean.TRUE.equals(StringUtils.mailIsValid(customerContactDTO.getEmail()))
						? customerContactDTO.getEmail()
						: defaultACMReceiverMail);
		customerContactDTO.setSubject("Contact Message from IB");

		// 1 : HAUT | 2 NORMAL
		customerContactDTO.setPriority(2);

		customerContactDTO.setRead(Boolean.FALSE);

		if (ACMValidationUtils.isNullOrEmpty(customerContactDTO.getLinkReplay())) {
			// setting default value to : 0
			customerContactDTO.setLinkReplay(0L);
		}
		else {
			customerContactDTO.setLinkReplay(customerContactDTO.getLinkReplay());
		}

		// setting default status to NEW
		customerContactDTO.setStatut(NotificationStatut.NEW.name());

		// mapping data
		CustomerContact customerContact = mapper.map(customerContactDTO, CustomerContact.class);
		CommonFunctions.mapperToSave(customerContact, userClient, logger);
		CustomerContact newCustomerContact = customerContactRepository.save(customerContact);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, CustomerContact.class.getSimpleName());
		CustomerContactDTO newCustomerContactDTO =
				mapper.map(newCustomerContact, CustomerContactDTO.class);

		// send mail
		sendMail(newCustomerContactDTO);
		return newCustomerContactDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerContactService#save(com.acm.utils.dtos.CustomerContactDTO)
	 */
	@Override
	public CustomerContactDTO saveMail(CustomerContactDTO customerContactDTO)
			throws CustomerContactException, ResourcesNotFoundException {

		Preconditions.checkNotNull(customerContactDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		CustomerDTO customerDTO = customerService.find(customerContactDTO.getCustomerId());

		AcmEnvironnementDTO acmEnvironmentDTO =
				parametrageClient.find(CommonConstants.INTERNET_BANKING);
		// if IB is enabled in AcmEnvironment && customer has an IB account then send ib message
		if (!ACMValidationUtils.isNullOrEmpty(acmEnvironmentDTO)
				&& acmEnvironmentDTO.getValue().equals("1")
				&& !ACMValidationUtils.isNullOrEmpty(customerDTO) && !ACMValidationUtils
						.isNullOrEmpty(userClient.findByLogin(customerDTO.getCustomerNumber()))) {

			// send IB message :
			// find connected user
			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);

			customerContactDTO.setFrom(userDTO.getEmail());
			customerContactDTO.setName(userDTO.getFullName());
			customerContactDTO.setUserName(userDTO.getLogin());

			// 1 : HAUT | 2 NORMAL
			customerContactDTO.setPriority(2);

			customerContactDTO.setRead(Boolean.FALSE);

			if (ACMValidationUtils.isNullOrEmpty(customerContactDTO.getLinkReplay())) {
				// setting default value to : 0
				customerContactDTO.setLinkReplay(0L);
			}
			else {
				customerContactDTO.setLinkReplay(customerContactDTO.getLinkReplay());
			}

			// setting default status to NEW
			customerContactDTO.setStatut(NotificationStatut.NEW.name());

			// mapping data
			CustomerContact customerContact = mapper.map(customerContactDTO, CustomerContact.class);
			CommonFunctions.mapperToSave(customerContact, userClient, logger);
			// send customer contact
			customerContactRepository.save(customerContact);

			logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
					CustomerContact.class.getSimpleName());
		}
		// else if IB is disabled in AcmEnvironment || customer has not ib account and has an email
		// then send mail
		else if (!ACMValidationUtils.isNullOrEmpty(customerDTO)
				&& !ACMValidationUtils.isNullOrEmpty(customerDTO.getEmail())) {

			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
			customerContactDTO.setFrom(userDTO.getEmail());
			sendMail(customerContactDTO);
			return customerContactDTO;
		}
		// else throw an exception to inform that this customer doesn't have IB and he doesn't have
		// an email
		else {
			throw new CustomerContactException(
					new ExceptionResponseMessage(CommonErrorCode.NO_CUSTOMER_CONTACT,
							CommonExceptionsMessage.UNABLE_TO_CONTACT_CUSTOMER),
					CommonExceptionsMessage.UNABLE_TO_CONTACT_CUSTOMER);
		}
		return customerContactDTO;
	}

	/**
	 * Send contact mail.
	 *
	 * @author Abdelkarim
	 * @param customerContactDTO the customer contact DTO
	 */
	private void sendMail(CustomerContactDTO customerContactDTO) {

		try {
			mailSenderClient.sendContactEmail(customerContactDTO);
			logger.info("Sending Email to customer :: DONE");
		}
		catch (FeignException e) {
			logger.error("Failed to send Mail");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerContactService#save(java.lang.Long,
	 * com.acm.utils.dtos.CustomerContactDTO)
	 */
	@Override
	public CustomerContactDTO save(Long id, CustomerContactDTO customerContactDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(customerContactDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Update CustomerContact with ID = {}", id);
		CustomerContact oldCustomerContact = customerContactRepository.findById(id).orElse(null);

		// check if object is null
		if (oldCustomerContact == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					CustomerContact.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + CustomerContact.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldCustomerContact)
		mapper.map(customerContactDTO, oldCustomerContact);
		CommonFunctions.mapperToUpdate(oldCustomerContact, userClient, logger);

		// update & persist data in DB
		CustomerContact newCustomerContact = customerContactRepository.save(oldCustomerContact);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, CustomerContact.class.getSimpleName());
		return mapper.map(newCustomerContact, CustomerContactDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.CustomerContactService#disableContact(com.acm.utils.dtos.CustomerContactDTO)
	 */
	@Override
	public void disableContact(CustomerContactDTO customerContactDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(customerContactDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(customerContactDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("disable customerContacts  with ID = {}", customerContactDTO.getId());
		// delete object by id
		CustomerContactDTO newCustomerContactDTO = find(customerContactDTO.getId());
		newCustomerContactDTO.setEnabled(Boolean.FALSE);
		save(newCustomerContactDTO.getId(), newCustomerContactDTO);
		logger.info("disable Message  with ID = {} :: DONE", customerContactDTO.getId());
	}
}
