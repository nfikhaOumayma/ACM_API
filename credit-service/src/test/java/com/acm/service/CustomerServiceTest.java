/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.ParametrageClient;
import com.acm.client.ReportingClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.CustomerRepository;
import com.acm.service.impl.CustomerServiceImpl;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.dtos.pagination.CustomerPaginationDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;
import com.acm.utils.models.Customer;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link CustomerServiceTest} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
class CustomerServiceTest {

	/** The customer service. */
	@InjectMocks
	private CustomerServiceImpl customerService;

	/** The customer repository. */
	@Mock
	private CustomerRepository customerRepository;

	/** The address service. */
	@Mock
	private AddressService addressService;

	/** The user defined fields links service. */
	@Mock
	private UserDefinedFieldsLinksService userDefinedFieldsLinksService;

	/** The customer links relationship service. */
	@Mock
	private CustomerLinksRelationshipService customerLinksRelationshipService;

	/** The loan service. */
	@Mock
	private LoanService loanService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The parametrage client. */
	@Mock
	private ParametrageClient parametrageClient;

	/** The transvers client. */
	@Mock
	private TransversClient transversClient;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The mail sender client. */
	@Mock
	private ReportingClient mailSenderClient;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Initialize an customer.
	 *
	 * @author HaythemBenizid
	 * @param customerType the customer type
	 * @return a new customer
	 */
	private Customer initCustomer(CustomerType customerType) {

		Customer customer = new Customer();
		customer.setAcmVersion(0);
		customer.setId(new Long(1));
		customer.setCustomerType(customerType.name());
		customer.setCustomerIdExtern(1L);
		return customer;
	}

	/**
	 * Initialize the customer.
	 *
	 * @author HaythemBenizid
	 * @param customerType the customer type
	 * @return the customer DTO
	 */
	private CustomerDTO initCustomerDTO(CustomerType customerType) {

		CustomerDTO customerDTO = new CustomerDTO();
		customerDTO.setId(1L);
		customerDTO.setCustomerType(customerType.name());
		customerDTO.setCustomerIdExtern(1L);
		return customerDTO;
	}

	/**
	 * Initialize a new UserDTO.
	 * 
	 * @author HaythemBenizid
	 * @return a new UserDTO
	 */
	private UserDTO initUserDTO() {

		UserDTO userDTO = new UserDTO();
		userDTO.setNom("test");
		userDTO.setEmail("test@test.gmail");
		userDTO.setLogin("login");
		userDTO.setPrenom("benTest");
		userDTO.setTypeUser(UserHierarchicalType.CONNECTED_USER.name());
		userDTO.setCategory(UserCategory.OPERATION.name());
		userDTO.setAccountPortfolioId(1L);
		return userDTO;
	}

	/**
	 * Should success find List customer.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessListFindCustomer() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());
		given(userClient.findUsers()).willReturn(Collections.singletonList(initUserDTO()));

		// WHEN
		List<CustomerDTO> result = customerService.find(customerDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find customer by ID.
	 * 
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindCustomerByID() throws ResourcesNotFoundException {

		// GIVEN
		given(customerRepository.findById(any(Long.class)))
				.willReturn(Optional.of(initCustomer(CustomerType.INDIV)));

		// WHEN
		CustomerDTO customerDTO = customerService.findCustomer(new Long(1));

		// THEN
		assertThat(customerDTO).isNotNull();
	}

	/**
	 * Should not success find customer by ID.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldNotSuccessFindCustomerByID() throws ResourcesNotFoundException {

		// GIVEN
		given(environment.getProperty(any(String.class)))
				.willReturn(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// WHEN
		try {
			customerService.findCustomer(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("Customer with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success find by ID.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindByID() throws ResourcesNotFoundException {

		// GIVEN
		CustomerDTO initCustomerDTO = initCustomerDTO(CustomerType.INDIV);
		given(customerRepository.findById(any(Long.class)))
				.willReturn(Optional.of(initCustomer(CustomerType.INDIV)));
		AddressDTO paramsAddress = new AddressDTO();
		paramsAddress.setCustomerId(initCustomerDTO.getId());
		given(addressService.find(paramsAddress)).willReturn(new ArrayList<>());
		UserDefinedFieldsLinksDTO paramsUDFLinksDTO = new UserDefinedFieldsLinksDTO();
		paramsUDFLinksDTO.setCustomerId(initCustomerDTO.getId());
		given(userDefinedFieldsLinksService.find(paramsUDFLinksDTO)).willReturn(new ArrayList<>());
		// WHEN
		CustomerDTO customerDTO = customerService.find(new Long(1));

		// THEN
		assertThat(customerDTO).isNotNull();
	}

	/**
	 * Should not success find by ID.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldNotSuccessFindByID() throws ResourcesNotFoundException {

		// GIVEN
		given(environment.getProperty(any(String.class)))
				.willReturn(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// WHEN
		try {
			customerService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("Customer with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success find by id extern.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindByIdExtern() throws ResourcesNotFoundException {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		given(customerRepository.findByCustomerIdExtern(any(Long.class)))
				.willReturn(Collections.singletonList(customer));
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		// WHEN
		List<CustomerDTO> resultCustomerDTOs = customerService.findCustomerIdExtern(1L, null);

		// THEN
		assertThat(resultCustomerDTOs).isNotNull();
	}

	/**
	 * Should not success find by id extern.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldNotSuccessFindByIdExtern() throws ResourcesNotFoundException {

		// GIVEN
		given(customerRepository.findByCustomerIdExtern(any(Long.class)))
				.willReturn(new ArrayList<>());
		// WHEN
		List<CustomerDTO> resultCustomerDTOs = customerService.findCustomerIdExtern(1L, null);

		// THEN
		assertThat(resultCustomerDTOs).isNotNull();
	}

	/**
	 * Should success save INDIV.
	 *
	 * @author HaythemBenizid
	 * @throws CalculateAgeException the calculate age exception
	 */
	@Test
	void shouldSuccessSaveINDIV() throws CalculateAgeException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		Customer customer = initCustomer(CustomerType.INDIV);

		given(customerRepository.save(any(Customer.class))).willReturn(customer);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		CustomerDTO customerDTOReponse = customerService.save(customerDTO);

		// THEN
		assertThat(customerDTOReponse).isNotNull();
	}

	/**
	 * Should success save INDIV with age calculate.
	 *
	 * @author HaythemBenizid
	 * @throws CalculateAgeException the calculate age exception
	 */
	@Test
	void shouldSuccessSaveINDIVWithAgeCalculate() throws CalculateAgeException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		Date dateOfBirth = new GregorianCalendar(1980, Calendar.FEBRUARY, 11).getTime();
		customerDTO.setDateOfBirth(dateOfBirth);
		Customer customer = initCustomer(CustomerType.INDIV);
		customer.setDateOfBirth(dateOfBirth);

		given(customerRepository.save(any(Customer.class))).willReturn(customer);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		CustomerDTO customerDTOReponse = customerService.save(customerDTO);

		// THEN
		assertThat(customerDTOReponse).isNotNull();
	}

	/**
	 * Should NOT success save INDIV with error age calculate.
	 *
	 * @author HaythemBenizid
	 * @throws CalculateAgeException the calculate age exception
	 */
	@Test
	void shouldNotSuccessSaveINDIVWithErrorAgeCalculate() throws CalculateAgeException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		customerDTO.setDateOfBirth(new GregorianCalendar(2030, Calendar.FEBRUARY, 11).getTime());
		Customer customer = initCustomer(CustomerType.INDIV);
		customer.setDateOfBirth(new GregorianCalendar(2030, Calendar.FEBRUARY, 11).getTime());

		given(customerRepository.save(any(Customer.class))).willReturn(customer);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		try {
			customerService.saveForApplication(customerDTO);
			Assert.fail("CalculateAgeException expected");
		}
		catch (CalculateAgeException expected) {
			String message = expected.getMessage();
			assertThat(message,
					containsString(CommonExceptionsMessage.CUSTOMER_INVALID_DATE_BIRTH));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success save GRP.
	 *
	 * @author HaythemBenizid
	 * @throws CalculateAgeException the calculate age exception
	 */
	@Test
	void shouldSuccessSaveGRP() throws CalculateAgeException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.GRP);
		Customer customer = initCustomer(CustomerType.GRP);

		given(customerRepository.save(any(Customer.class))).willReturn(customer);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		CustomerDTO customerDTOReponse = customerService.save(customerDTO);

		// THEN
		assertThat(customerDTOReponse).isNotNull();
	}

	/**
	 * Should success save ORG.
	 *
	 * @author HaythemBenizid
	 * @throws CalculateAgeException the calculate age exception
	 */
	@Test
	void shouldSuccessSaveORG() throws CalculateAgeException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.ORG);
		Customer customer = initCustomer(CustomerType.ORG);

		given(customerRepository.save(any(Customer.class))).willReturn(customer);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		CustomerDTO customerDTOReponse = customerService.save(customerDTO);

		// THEN
		assertThat(customerDTOReponse).isNotNull();
	}

	/**
	 * Should success update INDIV.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	@Test
	void shouldSuccessUpdateINDIV()
			throws ResourcesNotFoundException, CalculateAgeException, CreditException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		Date dateOfBirth = new GregorianCalendar(1980, Calendar.FEBRUARY, 11).getTime();
		customerDTO.setDateOfBirth(dateOfBirth);
		Customer customer = mapper.map(customerDTO, Customer.class);
		customer.setAcmVersion(0);
		given(customerRepository.findById(any(Long.class))).willReturn(Optional.of(customer));
		given(customerRepository.save(any(Customer.class))).willReturn(customer);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		CustomerDTO customerDTOReponse = customerService.save(customer.getId(), customerDTO);

		// THEN
		assertThat(customerDTOReponse).isNotNull();
	}

	/**
	 * Should not success update INDIV with error age calculate.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	@Test
	void shouldNotSuccessUpdateINDIVWithErrorAgeCalculate()
			throws ResourcesNotFoundException, CalculateAgeException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		customerDTO.setDateOfBirth(new GregorianCalendar(2030, Calendar.FEBRUARY, 11).getTime());
		Customer customer = mapper.map(customerDTO, Customer.class);
		customer.setAcmVersion(0);
		given(customerRepository.findById(any(Long.class))).willReturn(Optional.of(customer));
		given(customerRepository.saveAndFlush(any(Customer.class))).willReturn(customer);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		try {
			customerService.save(customerDTO.getId(), customerDTO);
			Assert.fail("CalculateAgeException expected");
		}
		catch (CalculateAgeException expected) {
			String message = expected.getMessage();
			assertThat(message,
					containsString(CommonExceptionsMessage.CUSTOMER_INVALID_DATE_BIRTH));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should not success update INDIV with error not found.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	@Test
	void shouldNotSuccessUpdateINDIVWithErrorNotFound()
			throws ResourcesNotFoundException, CalculateAgeException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		// WHEN
		try {
			customerService.save(customerDTO.getId(), customerDTO);
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("Customer with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success update GRP.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	@Test
	void shouldSuccessUpdateGRP()
			throws ResourcesNotFoundException, CalculateAgeException, CreditException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.GRP);
		Customer customer = mapper.map(customerDTO, Customer.class);
		customer.setAcmVersion(0);
		given(customerRepository.findById(any(Long.class))).willReturn(Optional.of(customer));
		given(customerRepository.save(any(Customer.class))).willReturn(customer);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		CustomerDTO customerDTOReponse = customerService.save(customer.getId(), customerDTO);

		// THEN
		assertThat(customerDTOReponse).isNotNull();
	}

	/**
	 * Should success update ORG.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	@Test
	void shouldSuccessUpdateORG()
			throws ResourcesNotFoundException, CalculateAgeException, CreditException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.ORG);
		Customer customer = mapper.map(customerDTO, Customer.class);
		customer.setAcmVersion(0);
		given(customerRepository.findById(any(Long.class))).willReturn(Optional.of(customer));
		given(customerRepository.save(any(Customer.class))).willReturn(customer);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		CustomerDTO customerDTOReponse = customerService.save(customer.getId(), customerDTO);

		// THEN
		assertThat(customerDTOReponse).isNotNull();
	}

	/**
	 * Should success find customers for connected user OPERATION.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomersForConnectedUserOPERATION() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		UserDTO connectedUser = initUserDTO();
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(connectedUser);
		given(userClient.findUsers()).willReturn(Collections.singletonList(connectedUser));
		given(customerRepository.findAll(any(BooleanBuilder.class))).willReturn(new ArrayList<>());

		// WHEN
		List<CustomerDTO> result = customerService.findCustomers();

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find customers for connected user MANAGMENT.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomersForConnectedUserMANAGMENT() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		UserDTO connectedUser = initUserDTO();
		connectedUser.setBranchID(1);
		connectedUser.setCategory(UserCategory.MANAGMENT.name());
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(connectedUser);
		given(userClient.findUsers()).willReturn(Collections.singletonList(connectedUser));
		given(customerRepository.findAll(any(BooleanBuilder.class))).willReturn(new ArrayList<>());

		// WHEN
		List<CustomerDTO> result = customerService.findCustomers();

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find customers for connected user MANAGMENT with access branches.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomersForConnectedUserMANAGMENTWithAccessBranches() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		UserDTO connectedUser = initUserDTO();
		connectedUser.setCategory(UserCategory.MANAGMENT.name());
		connectedUser.setBranchID(1);
		connectedUser.setAccessBranches("1,2,3");
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(connectedUser);
		given(userClient.findUsers()).willReturn(Collections.singletonList(connectedUser));
		given(customerRepository.findAll(any(BooleanBuilder.class))).willReturn(new ArrayList<>());

		// WHEN
		List<CustomerDTO> result = customerService.findCustomers();

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find customers pagination for connected user OPERATION.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomersPaginationForConnectedUserOPERATION() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		CustomerPaginationDTO customerPaginationDTO = new CustomerPaginationDTO();
		customerPaginationDTO.setParams(new CustomerDTO());
		UserDTO connectedUser = initUserDTO();
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(connectedUser);
		given(userClient.findUsers()).willReturn(Collections.singletonList(connectedUser));
		Page<Customer> pagedResult = new Page<Customer>() {

			@Override
			public int getNumber() {

				return 0;
			}

			@Override
			public int getSize() {

				return 0;
			}

			@Override
			public int getNumberOfElements() {

				return 0;
			}

			@Override
			public List<Customer> getContent() {

				return Collections.singletonList(customer);
			}

			@Override
			public boolean hasContent() {

				return true;
			}

			@Override
			public Sort getSort() {

				return null;
			}

			@Override
			public boolean isFirst() {

				return false;
			}

			@Override
			public boolean isLast() {

				return false;
			}

			@Override
			public boolean hasNext() {

				return false;
			}

			@Override
			public boolean hasPrevious() {

				return false;
			}

			@Override
			public Pageable nextPageable() {

				return null;
			}

			@Override
			public Pageable previousPageable() {

				return null;
			}

			@Override
			public Iterator<Customer> iterator() {

				return null;
			}

			@Override
			public int getTotalPages() {

				return 2;
			}

			@Override
			public long getTotalElements() {

				return 10;
			}

			@Override
			public <U> Page<U> map(Function<? super Customer, ? extends U> converter) {

				return null;
			}
		};
		given(customerRepository.findAll(any(BooleanBuilder.class), any(Pageable.class)))
				.willReturn(pagedResult);

		// WHEN
		CustomerPaginationDTO result = customerService.find(customerPaginationDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find customers pagination for connected user OPERATION with all param.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomersPaginationForConnectedUserOPERATIONWithAllParam() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		CustomerPaginationDTO customerPaginationDTO = new CustomerPaginationDTO();
		CustomerDTO param = new CustomerDTO();
		param.setCustomerNumber("00123");
		param.setCustomerName("test test");
		param.setIdentity("123456789");
		param.setSolidarityName("group");
		param.setBranchesName("branch");
		param.setAccountPortfolioDescription("accountProtfolio");
		param.setCustomerType("INDIV");
		customerPaginationDTO.setParams(param);
		UserDTO connectedUser = initUserDTO();
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(connectedUser);
		given(userClient.findUsers()).willReturn(Collections.singletonList(connectedUser));
		Page<Customer> pagedResult = new Page<Customer>() {

			@Override
			public int getNumber() {

				return 0;
			}

			@Override
			public int getSize() {

				return 0;
			}

			@Override
			public int getNumberOfElements() {

				return 0;
			}

			@Override
			public List<Customer> getContent() {

				return Collections.singletonList(customer);
			}

			@Override
			public boolean hasContent() {

				return true;
			}

			@Override
			public Sort getSort() {

				return null;
			}

			@Override
			public boolean isFirst() {

				return false;
			}

			@Override
			public boolean isLast() {

				return false;
			}

			@Override
			public boolean hasNext() {

				return false;
			}

			@Override
			public boolean hasPrevious() {

				return false;
			}

			@Override
			public Pageable nextPageable() {

				return null;
			}

			@Override
			public Pageable previousPageable() {

				return null;
			}

			@Override
			public Iterator<Customer> iterator() {

				return null;
			}

			@Override
			public int getTotalPages() {

				return 2;
			}

			@Override
			public long getTotalElements() {

				return 10;
			}

			@Override
			public <U> Page<U> map(Function<? super Customer, ? extends U> converter) {

				return null;
			}
		};
		given(customerRepository.findAll(any(BooleanBuilder.class), any(Pageable.class)))
				.willReturn(pagedResult);

		// WHEN
		CustomerPaginationDTO result = customerService.find(customerPaginationDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find customers pagination for connected user OPERATION with all param sort
	 * DESC.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomersPaginationForConnectedUserOPERATIONWithAllParamSortDESC() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		CustomerPaginationDTO customerPaginationDTO = new CustomerPaginationDTO();
		CustomerDTO param = new CustomerDTO();
		param.setCustomerNumber("00123");
		param.setCustomerName("test test");
		param.setIdentity("123456789");
		param.setSolidarityName("group");
		param.setBranchesName("branch");
		param.setAccountPortfolioDescription("accountProtfolio");
		param.setCustomerType("INDIV");
		customerPaginationDTO.setParams(param);
		customerPaginationDTO.setSortDirection("-1");
		customerPaginationDTO.setSortField("customerNameNoPipe");
		UserDTO connectedUser = initUserDTO();
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(connectedUser);
		given(userClient.findUsers()).willReturn(Collections.singletonList(connectedUser));
		Page<Customer> pagedResult = new Page<Customer>() {

			@Override
			public int getNumber() {

				return 0;
			}

			@Override
			public int getSize() {

				return 0;
			}

			@Override
			public int getNumberOfElements() {

				return 0;
			}

			@Override
			public List<Customer> getContent() {

				return Collections.singletonList(customer);
			}

			@Override
			public boolean hasContent() {

				return true;
			}

			@Override
			public Sort getSort() {

				return null;
			}

			@Override
			public boolean isFirst() {

				return false;
			}

			@Override
			public boolean isLast() {

				return false;
			}

			@Override
			public boolean hasNext() {

				return false;
			}

			@Override
			public boolean hasPrevious() {

				return false;
			}

			@Override
			public Pageable nextPageable() {

				return null;
			}

			@Override
			public Pageable previousPageable() {

				return null;
			}

			@Override
			public Iterator<Customer> iterator() {

				return null;
			}

			@Override
			public int getTotalPages() {

				return 2;
			}

			@Override
			public long getTotalElements() {

				return 10;
			}

			@Override
			public <U> Page<U> map(Function<? super Customer, ? extends U> converter) {

				return null;
			}
		};
		given(customerRepository.findAll(any(BooleanBuilder.class), any(Pageable.class)))
				.willReturn(pagedResult);

		// WHEN
		CustomerPaginationDTO result = customerService.find(customerPaginationDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find customers pagination for connected user OPERATION with all param sort
	 * ASC.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomersPaginationForConnectedUserOPERATIONWithAllParamSortASC() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		CustomerPaginationDTO customerPaginationDTO = new CustomerPaginationDTO();
		CustomerDTO param = new CustomerDTO();
		param.setCustomerNumber("00123");
		param.setCustomerName("test test");
		param.setIdentity("123456789");
		param.setSolidarityName("group");
		param.setBranchesName("branch");
		param.setAccountPortfolioDescription("accountProtfolio");
		param.setCustomerType("INDIV");
		customerPaginationDTO.setParams(param);
		customerPaginationDTO.setSortDirection("1");
		customerPaginationDTO.setSortField("customerNameNoPipe");
		UserDTO connectedUser = initUserDTO();
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(connectedUser);
		given(userClient.findUsers()).willReturn(Collections.singletonList(connectedUser));
		Page<Customer> pagedResult = new Page<Customer>() {

			@Override
			public int getNumber() {

				return 0;
			}

			@Override
			public int getSize() {

				return 0;
			}

			@Override
			public int getNumberOfElements() {

				return 0;
			}

			@Override
			public List<Customer> getContent() {

				return Collections.singletonList(customer);
			}

			@Override
			public boolean hasContent() {

				return true;
			}

			@Override
			public Sort getSort() {

				return null;
			}

			@Override
			public boolean isFirst() {

				return false;
			}

			@Override
			public boolean isLast() {

				return false;
			}

			@Override
			public boolean hasNext() {

				return false;
			}

			@Override
			public boolean hasPrevious() {

				return false;
			}

			@Override
			public Pageable nextPageable() {

				return null;
			}

			@Override
			public Pageable previousPageable() {

				return null;
			}

			@Override
			public Iterator<Customer> iterator() {

				return null;
			}

			@Override
			public int getTotalPages() {

				return 2;
			}

			@Override
			public long getTotalElements() {

				return 10;
			}

			@Override
			public <U> Page<U> map(Function<? super Customer, ? extends U> converter) {

				return null;
			}
		};
		given(customerRepository.findAll(any(BooleanBuilder.class), any(Pageable.class)))
				.willReturn(pagedResult);

		// WHEN
		CustomerPaginationDTO result = customerService.find(customerPaginationDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find customers pagination for connected user MANAGMENT.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomersPaginationForConnectedUserMANAGMENT() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		CustomerPaginationDTO customerPaginationDTO = new CustomerPaginationDTO();
		customerPaginationDTO.setParams(new CustomerDTO());
		UserDTO connectedUser = initUserDTO();
		connectedUser.setCategory(UserCategory.MANAGMENT.name());
		connectedUser.setBranchID(1);
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(connectedUser);
		given(userClient.findUsers()).willReturn(Collections.singletonList(connectedUser));
		Page<Customer> pagedResult = new Page<Customer>() {

			@Override
			public int getNumber() {

				return 0;
			}

			@Override
			public int getSize() {

				return 0;
			}

			@Override
			public int getNumberOfElements() {

				return 0;
			}

			@Override
			public List<Customer> getContent() {

				return Collections.singletonList(customer);
			}

			@Override
			public boolean hasContent() {

				return true;
			}

			@Override
			public Sort getSort() {

				return null;
			}

			@Override
			public boolean isFirst() {

				return false;
			}

			@Override
			public boolean isLast() {

				return false;
			}

			@Override
			public boolean hasNext() {

				return false;
			}

			@Override
			public boolean hasPrevious() {

				return false;
			}

			@Override
			public Pageable nextPageable() {

				return null;
			}

			@Override
			public Pageable previousPageable() {

				return null;
			}

			@Override
			public Iterator<Customer> iterator() {

				return null;
			}

			@Override
			public int getTotalPages() {

				return 2;
			}

			@Override
			public long getTotalElements() {

				return 10;
			}

			@Override
			public <U> Page<U> map(Function<? super Customer, ? extends U> converter) {

				return null;
			}
		};
		given(customerRepository.findAll(any(BooleanBuilder.class), any(Pageable.class)))
				.willReturn(pagedResult);

		// WHEN
		CustomerPaginationDTO result = customerService.find(customerPaginationDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find customers pagination for connected user MANAGMENT with access branches.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomersPaginationForConnectedUserMANAGMENTWithAccessBranches() {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		CustomerPaginationDTO customerPaginationDTO = new CustomerPaginationDTO();
		customerPaginationDTO.setParams(new CustomerDTO());
		UserDTO connectedUser = initUserDTO();
		connectedUser.setCategory(UserCategory.MANAGMENT.name());
		connectedUser.setBranchID(1);
		connectedUser.setAccessBranches("1,2,3");
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(userClient.find()).willReturn(connectedUser);
		given(userClient.findUsers()).willReturn(Collections.singletonList(connectedUser));
		Page<Customer> pagedResult = new Page<Customer>() {

			@Override
			public int getNumber() {

				return 0;
			}

			@Override
			public int getSize() {

				return 0;
			}

			@Override
			public int getNumberOfElements() {

				return 0;
			}

			@Override
			public List<Customer> getContent() {

				return Collections.singletonList(customer);
			}

			@Override
			public boolean hasContent() {

				return true;
			}

			@Override
			public Sort getSort() {

				return null;
			}

			@Override
			public boolean isFirst() {

				return false;
			}

			@Override
			public boolean isLast() {

				return false;
			}

			@Override
			public boolean hasNext() {

				return false;
			}

			@Override
			public boolean hasPrevious() {

				return false;
			}

			@Override
			public Pageable nextPageable() {

				return null;
			}

			@Override
			public Pageable previousPageable() {

				return null;
			}

			@Override
			public Iterator<Customer> iterator() {

				return null;
			}

			@Override
			public int getTotalPages() {

				return 2;
			}

			@Override
			public long getTotalElements() {

				return 10;
			}

			@Override
			public <U> Page<U> map(Function<? super Customer, ? extends U> converter) {

				return null;
			}
		};
		given(customerRepository.findAll(any(BooleanBuilder.class), any(Pageable.class)))
				.willReturn(pagedResult);

		// WHEN
		CustomerPaginationDTO result = customerService.find(customerPaginationDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should not success find customer account.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldNotSuccessFindCustomerAccount() {

		// GIVEN
		given(transversClient.findCustomerAccountByCustomer(any(Long.class)))
				.willReturn(new ArrayList<>());
		// WHEN
		List<CustomerAccountDTO> resultCustomerDTOs = customerService.findCustomerAccount(1L);

		// THEN
		assertThat(resultCustomerDTOs).isNotNull();
	}

	/**
	 * Should success find customers relation ship.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindCustomersRelationShip() throws ResourcesNotFoundException {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs = new ArrayList<>();
		customerLinksRelationshipDTOs.add(new CustomerLinksRelationshipDTO(customerDTO, 1L));
		given(mapper.map(customer, CustomerDTO.class)).willReturn(customerDTO);
		given(customerLinksRelationshipService.find(any(CustomerLinksRelationshipDTO.class)))
				.willReturn(customerLinksRelationshipDTOs);

		// WHEN
		List<CustomerDTO> result = customerService.findCustomersRelationShip(customerDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should not success find customer active account.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldNotSuccessFindCustomerActiveAccount() {

		// GIVEN
		given(transversClient.findCustomerActiveAccount(any(Long.class), any(Long.class)))
				.willReturn(1L);
		// WHEN
		Long customerActiveAccount = customerService.findCustomerActiveAccount(1L, 2L);

		// THEN
		assertThat(customerActiveAccount).isNotNull();
	}

	/**
	 * Should not success find all active accounts for customer.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldNotSuccessFindAllActiveAccountsForCustomer() {

		// GIVEN
		CustomerActiveAccountDTO customerActiveAccountDTO = new CustomerActiveAccountDTO();
		given(transversClient.findAllActiveAccountsForCustomer(any(Long.class)))
				.willReturn(Collections.singletonList(customerActiveAccountDTO));
		// WHEN
		List<CustomerActiveAccountDTO> result =
				customerService.findAllActiveAccountsForCustomer(1L);

		// THEN
		assertThat(result).isNotNull();
		assertTrue(result.size() > 0);
	}

	/**
	 * Should not success find arrears customer.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldNotSuccessFindArrearsCustomer() {

		// GIVEN
		given(customerRepository.findByCustomerIdExtern(any(Long.class)))
				.willReturn(new ArrayList<>());
		given(customerService.findCustomerIdExtern(any(Long.class), null))
				.willReturn(new ArrayList<>());
		given(transversClient.findArrearsByCustomer(any(Long.class)))
				.willReturn(new ArrearsDTO(1L, 2L, 3L));

		// WHEN
		ArrearsDTO arrearsDTO = customerService.findArrearsCustomer(1L);

		// THEN
		assertThat(arrearsDTO).isNotNull();
		assertThat(arrearsDTO.getCustomerIdExtern()).isNotNull();
	}

	/**
	 * Should success resend login send mail.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessResendLoginSendMail() throws ResourcesNotFoundException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		customerDTO.setCustomerNumber("123456");
		customerDTO.setEmail("mail@test.com");
		UserDTO connectedUser = initUserDTO();
		given(userClient.findByLogin(any(String.class))).willReturn(connectedUser);
		doNothing().when(mailSenderClient).sendEmail(any(MailCustomerDTO.class));
		// WHEN
		UserDTO result = customerService.resendLogin(customerDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success resend login create and send mail INDIV or ORG.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessResendLoginCreateAndSendMailINDIVOrORG() throws ResourcesNotFoundException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);
		customerDTO.setCustomerNumber("123456");
		customerDTO.setEmail("mail@test.com");
		UserDTO connectedUser = initUserDTO();
		connectedUser.setPwd("wxcv");
		AcmEnvironnementDTO acmEnvironmentDTO =
				new AcmEnvironnementDTO(CommonConstants.INTERNET_BANKING, "1");
		given(userClient.findByLogin(any(String.class))).willReturn(null);
		given(parametrageClient.find(any(String.class))).willReturn(acmEnvironmentDTO);
		given(userClient.createForIB(any(UserDTO.class))).willReturn(connectedUser);
		doNothing().when(mailSenderClient).sendEmail(any(MailCustomerDTO.class));
		// WHEN
		UserDTO result = customerService.resendLogin(customerDTO);

		// THEN
		Assertions.assertThat(result).isNull();
	}

	/**
	 * Should success resend login create and send mail GRP.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessResendLoginCreateAndSendMailGRP() throws ResourcesNotFoundException {

		// GIVEN
		CustomerDTO customerDTOGRP = initCustomerDTO(CustomerType.GRP);
		customerDTOGRP.setCustomerNumber("123456");
		customerDTOGRP.setEmail("mail@test.com");
		CustomerDTO customerDTOINDIV = initCustomerDTO(CustomerType.INDIV);
		customerDTOINDIV.setCustomerNumber("123456");
		customerDTOINDIV.setEmail("mail@test.com");
		UserDTO connectedUser = initUserDTO();
		connectedUser.setPwd("wxcv");
		AcmEnvironnementDTO acmEnvironmentDTO =
				new AcmEnvironnementDTO(CommonConstants.INTERNET_BANKING, "1");
		List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs = new ArrayList<>();
		customerLinksRelationshipDTOs.add(new CustomerLinksRelationshipDTO(customerDTOGRP, 1L));
		given(userClient.findByLogin(any(String.class))).willReturn(null);
		given(parametrageClient.find(any(String.class))).willReturn(acmEnvironmentDTO);
		given(userClient.createForIB(any(UserDTO.class))).willReturn(connectedUser);
		given(customerService.findCustomersRelationShip(customerDTOGRP))
				.willReturn(Collections.singletonList(customerDTOINDIV));
		given(customerLinksRelationshipService.find(any(CustomerLinksRelationshipDTO.class)))
				.willReturn(customerLinksRelationshipDTOs);
		doNothing().when(mailSenderClient).sendEmail(any(MailCustomerDTO.class));
		// WHEN
		UserDTO result = customerService.resendLogin(customerDTOGRP);

		// THEN
		Assertions.assertThat(result).isNull();
	}

	/**
	 * Should success find photo customer.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindPhotoCustomer() throws ResourcesNotFoundException {

		// GIVEN
		Customer customer = initCustomer(CustomerType.INDIV);
		customer.setPhoto(new byte[1]);
		given(customerRepository.findById(any(Long.class))).willReturn(Optional.of(customer));

		// WHEN
		byte[] photo = customerService.findPhotoCustomer("1");

		// THEN
		assertThat(photo).isNotNull();
	}

	/**
	 * Should not success find photo customer.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldNotSuccessFindPhotoCustomer() throws ResourcesNotFoundException {

		// GIVEN
		given(environment.getProperty(any(String.class)))
				.willReturn(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// WHEN
		try {
			customerService.findPhotoCustomer("1");
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("Customer with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success check customer loan statuts.
	 * 
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessCheckCustomerLoanStatuts() throws ResourcesNotFoundException {

		// GIVEN
		CustomerDTO customerDTO = initCustomerDTO(CustomerType.INDIV);

		// WHEN
		Boolean status = customerService.checkCustomerLoanStatuts(customerDTO);

		// THEN
		assertThat(status).isNotNull();
	}

	/**
	 * Should success find customer paid account.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindCustomerPaidAccount() {

		// GIVEN
		given(transversClient.findCustomerPaidAccount(any(Long.class), any(Long.class)))
				.willReturn(0d);
		// WHEN
		Double result = customerService.findCustomerPaidAccount(1L, 2L);

		// THEN
		assertThat(result).isNotNull();
	}

}
