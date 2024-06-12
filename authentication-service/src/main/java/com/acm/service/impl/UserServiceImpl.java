/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.client.ReportingClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.CustomException;
import com.acm.exceptions.type.OldPwdInvalidException;
import com.acm.exceptions.type.PwdConfirmInvalidException;
import com.acm.exceptions.type.ResetPwdException;
import com.acm.repository.UserRepository;
import com.acm.service.UserService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.UserPaginationDTO;
import com.acm.utils.enums.MailBuilderMethod;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;
import com.acm.utils.enums.UserLangValues;
import com.acm.utils.models.Groupe;
import com.acm.utils.models.QUser;
import com.acm.utils.models.User;
import com.acm.utils.string.StringUtils;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

import feign.FeignException;

/**
 * {@link UserServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Service
public class UserServiceImpl implements UserService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(UserServiceImpl.class);

	/** The pwd expire delay. */
	private Integer pwdExpireDelay = null;

	/** The Constant USER_PWD_EXPIRE_DELAY. */
	private static final String USER_PWD_EXPIRE_DELAY = "USER_PWD_EXPIRE_DELAY";

	/** The Constant DEFAULT_PWD_EXPIRE_DELAY. */
	private static final Integer DEFAULT_PWD_EXPIRE_DELAY = 90;

	/** The Constant MAX_DEEP_SEARCH_RESPONSABLE. */
	private static final Integer MAX_DEEP_SEARCH_RESPONSABLE = 5;

	/** The user repository. */
	@Autowired
	private UserRepository userRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** the BCryptPasswordEncoder. */
	@Autowired
	private BCryptPasswordEncoder cryptPasswordEncoder;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#find(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public List<UserDTO> find(UserDTO userDTO) {

		// init QUser
		QUser qUser = QUser.user;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only not CUSTOMER user
		predicate.and(qUser.category.ne(UserCategory.CUSTOMER.name()));
		// find only not acm batch
		predicate.and(qUser.username.ne("acmbatch"));
		// find only not super admin
		predicate.and(qUser.username.ne(CommonConstants.DEFAULT_USER));

		// find by login
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getLogin())) {
			predicate.and(qUser.username.eq(userDTO.getLogin()));
		}

		// find by responsible
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getResponsableId())) {
			predicate.and(qUser.responsableId.eq(userDTO.getResponsableId()));
		}

		// find by UserProfilId
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getUserProfilId())) {
			predicate.and(qUser.userProfilId.eq(userDTO.getUserProfilId()));
		}

		// find by UserExternId (used in Batch)
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getUserExternId())) {
			predicate.and(qUser.userExternId.eq(userDTO.getUserExternId()));
		}

		// find by AccountPortfolioId
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getAccountPortfolioId())) {
			predicate.and(qUser.accountPortfolioId.eq(userDTO.getAccountPortfolioId()));
		}

		// find by branch id
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getBranchID())) {
			predicate.and(qUser.branchID.eq(userDTO.getBranchID()));
		}
		BooleanBuilder subPredicate = new BooleanBuilder();
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			subPredicate.and(qUser.accessBranches.like("%," + userDTO.getAccessBranches() + ",%"));
			subPredicate.or(qUser.accessBranches.like("%," + userDTO.getAccessBranches()));
			subPredicate.or(qUser.accessBranches.like(userDTO.getAccessBranches() + ",%"));
			subPredicate.or(qUser.accessBranches.eq(userDTO.getAccessBranches()));
		}
		predicate.and(subPredicate);
		// find Users by category
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getCategory())) {
			predicate.and(qUser.category.eq(userDTO.getCategory()));
		}
		// find by employeeId
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getEmployeeId())) {
			predicate.and(qUser.employeeId.eq(userDTO.getEmployeeId()));
		}
		Iterable<User> iterable = userRepository.findAll(predicate);
		List<User> users = new ArrayList<>();
		iterable.forEach(users::add);
		logger.info("{} : User was founded", users.size());

		// mapping data
		List<UserDTO> userDTOs = new ArrayList<>();
		users.forEach(user -> userDTOs.add(mapper.map(user, UserDTO.class)));
		return userDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#find(java.lang.String)
	 */
	@Override
	public UserDTO find(String login) {

		Preconditions.checkNotNull(login, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		User user = userRepository.findByUsernameIgnoreCase(login);
		if (!ACMValidationUtils.isNullOrEmpty(user)) {
			return mapper.map(user, UserDTO.class);
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#save(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO save(UserDTO userDTO) throws CustomException {

		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check if user exist by login
		UserDTO checkUser = find(userDTO.getLogin());
		if (checkUser != null) {
			logger.warn("### User with User_name : {} is already exists in DB ###",
					checkUser.getLogin());
			checkUser.setNewEntry(Boolean.FALSE);
			return checkUser;
		}

		// mapping & start inserting data
		User user = mapper.map(userDTO, User.class);

		// find && setting connected user details
		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		UserDTO connectedUser = (UserDTO) authentication.getPrincipal();
		user.setInsertBy(connectedUser.getLogin());
		// setting system data
		user.setAcmVersion(0);
		user.setDateInsertion(new Date());
		user.setEnabled(Boolean.FALSE);
		// force user to change his PWD after first login
		user.setTemporaryPwd(Boolean.TRUE);
		// --- 0 : DEFAULT => NO PORTFOLIO COTÉ ABACUS
		user.setAccountPortfolioId(
				user.getAccountPortfolioId() != null ? user.getAccountPortfolioId() : 0);
		// --- 0 : DEFAULT => NO USER COTÉ ABACUS
		user.setUserExternId(user.getUserExternId() != null ? user.getUserExternId() : 0);
		// --- 0 : DEFAULT => NO PROFIL COTÉ ABACUS
		user.setUserProfilId(user.getUserProfilId() != null ? user.getUserProfilId() : 0);
		// --- 0 : DEFAULT => NO CUSTOMER ID FOR ACM USERS
		user.setCustomerId(0L);
		// DEFAULT CATEGORY => OPERATION
		user.setCategory(
				user.getCategory() != null ? user.getCategory() : UserCategory.OPERATION.name());
		// --- "" :DEFAULT PORTFOLIO_NAME
		user.setPortfolioName(user.getPortfolioName() != null ? user.getPortfolioName() : "");
		// DEFAULT DEFAULT LANG => AR
		user.setDefaultLang(
				user.getDefaultLang() != null ? user.getDefaultLang() : UserLangValues.AR.name());
		// SET RESIGNING DATE
		user.setResigningDate(userDTO.getResigningDate());
		// SET HIRING DATE
		user.setHiringDate(userDTO.getHiringDate());
		// SET EMPLOYEE ID
		user.setEmployeeId(userDTO.getEmployeeId());

		// --- 0 : DEFAULT => NO RESPONSABLE FOR ACM USERS
		if (ACMValidationUtils.isNullOrEmpty(user.getResponsableId())) {
			// assign to super.admin (ADMIN)
			user.setResponsableId(CommonConstants.DEFAULT_USER);
		}

		// setting encrypted pwd
		String pwd = CommonFunctions.generateRandomString(8);
		// set pwd to userDTO that will be sent in the mail informing the new user his login and pwd
		userDTO.setPwd(pwd);

		user.setPassword(cryptPasswordEncoder.encode(pwd));
		user.setPwdExpiryDate(DateUtil.addDays(new Date(), getPwdExpireDelay()));

		// affect user to groupe using USER_PROFILE_ID_EXTERN
		user.setGroupes(null);
		assignGroupeUser(user, Boolean.FALSE, userDTO.getGroupes());

		try {
			// insert new user
			User newUser = userRepository.save(user);
			logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, User.class.getSimpleName());

			// send mail
			sendMail(new MailCustomerDTO(userDTO, userDTO.getLogin(), userDTO.getPwd(), new MailDTO(
					CommonConstants.NO_REPLAY_EMAIL,
					(!ACMValidationUtils.isNullOrEmpty(userDTO.getEmail())
							&& Boolean.TRUE.equals(StringUtils.mailIsValid(userDTO.getEmail())))
									? userDTO.getEmail()
									: defaultACMReceiverMail,
					"Loan Application Accepted", ""), MailBuilderMethod.BUILD_ADD_USER));
			return mapper.map(newUser, UserDTO.class);
		}
		catch (DataIntegrityViolationException e) {
			if (e.getCause().getCause().toString().contains("EMPLOYEE_ID")) {
				logger.error("Save User Exception : EMPLOYEE_ID duplicated ");
				throw new CustomException(CommonErrorCode.CODE_USER_EXIST);
			}
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#saveForBatch(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO saveForBatch(UserDTO userDTO) {

		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check if user exist in ACM with ID USER from ABACUS
		UserDTO userParams = new UserDTO();
		userParams.setUserExternId(userDTO.getUserExternId());
		List<UserDTO> checkUsers = find(userParams);
		if (!ACMValidationUtils.isNullOrEmpty(checkUsers)) {
			logger.warn("### User with UserExternId : {} is already exists in ACM DB ###",
					checkUsers.get(0).getUserExternId());
			checkUsers.get(0).setNewEntry(Boolean.FALSE);
			if (!checkUsers.get(0).getLogin().equals(userDTO.getLogin())) {
				// disable founded user
				User user = userRepository.findByUsernameIgnoreCase(checkUsers.get(0).getLogin());
				user.setEnabled(Boolean.FALSE);
				// force user to change his PWD after first login
				user.setTemporaryPwd(Boolean.TRUE);
				// setting category
				user.setCategory(UserCategory.OPERATION.name());
				// DEFAULT DEFAULT LANG => AR
				user.setDefaultLang(UserLangValues.AR.name());
				// update user
				User updatedUser = userRepository.save(user);
				return mapper.map(updatedUser, UserDTO.class);
			}
			else {
				return checkUsers.get(0);
			}
		}

		// setting category
		userDTO.setCategory(UserCategory.OPERATION.name());
		// DEFAULT DEFAULT LANG => AR
		userDTO.setDefaultLang(UserLangValues.AR.name());
		// call method save in ACM
		UserDTO newUserDTO = null;
		try {
			newUserDTO = save(userDTO);
			newUserDTO.setNewEntry(Boolean.TRUE);
		}
		catch (CustomException e) {
			logger.error("Save User failed because of a unicity check on Employee ID !!");
		}
		logger.info("Executing Method saveForBatch() :: DONE");
		return newUserDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#save(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO saveForIB(UserDTO userDTO) {

		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (ACMValidationUtils.isNullOrEmpty(userDTO.getLogin())) {
			// find by id ACM
			CustomerDTO customerDTO = creditClient.findCustomerById(userDTO.getCustomerId());
			userDTO.setLogin(customerDTO.getCustomerNumber());
		}
		// check if user exist by login
		UserDTO checkUser = find(userDTO.getLogin());
		if (checkUser != null) {
			logger.warn("### User with User_name : {} is already exists in DB ###",
					checkUser.getLogin());
			return checkUser;
		}

		// mapping & start inserting data
		User user = mapper.map(userDTO, User.class);

		// find && setting connected user details
		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		UserDTO connectedUser = (UserDTO) authentication.getPrincipal();
		user.setInsertBy(connectedUser.getLogin());
		// setting system data
		user.setAcmVersion(0);
		user.setDateInsertion(new Date());
		user.setEnabled(Boolean.TRUE);
		user.setAccountPortfolioId(0L);
		user.setUserExternId(0L);
		user.setUserProfilId(0L);
		// --- -1 : DEFAULT => NO RESPONSABLE FOR IB USERS
		user.setResponsableId("-1");
		// setting category to CUSTOMER
		user.setCategory(UserCategory.CUSTOMER.name());
		// DEFAULT DEFAULT LANG => AR
		userDTO.setDefaultLang(UserLangValues.AR.name());
		// generate random PWD
		String pwd = CommonFunctions.generateRandomString(8);
		logger.info("New Sending Password = {}", pwd);
		// setting encrypted pwd
		user.setPassword(cryptPasswordEncoder.encode(pwd));
		user.setPwdExpiryDate(DateUtil.addDays(new Date(), getPwdExpireDelay()));

		user.setTemporaryPwd(Boolean.TRUE);

		// affect user to groupe using USER_PROFILE_ID_EXTERN
		user.setGroupes(null);
		assignGroupeUser(user, Boolean.TRUE, null);

		// insert new user
		User newUser = userRepository.save(user);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, User.class.getSimpleName());
		UserDTO createdUserDTO = mapper.map(newUser, UserDTO.class);
		createdUserDTO.setPwd(pwd);

		logger.info("Executing Method saveForIB() :: DONE");
		return createdUserDTO;
	}

	/**
	 * Assign groupe user.
	 * 
	 * @author idridi
	 * @param user the user
	 * @param ibUser the ib user
	 * @param groupes the groupes
	 * @return the user
	 */
	private User assignGroupeUser(User user, Boolean ibUser, Set<GroupeDTO> groupes) {

		GroupeDTO paramsGroupeDTO = new GroupeDTO();
		List<GroupeDTO> groupeDTOs = ACMValidationUtils.isNullOrEmpty(groupes) ? new ArrayList<>()
				: new ArrayList<>(groupes);
		if (Boolean.TRUE.equals(ibUser)) {
			// default group for IB users
			paramsGroupeDTO.setCode("IB_GROUP");
			groupeDTOs = parametrageClient.find(paramsGroupeDTO);
		}
		else if (ACMValidationUtils.isNullOrEmpty(groupeDTOs)) {
			// by default assign user created to ACM_DEFAULT_GROUP Group
			paramsGroupeDTO.setCode("ACM_DEFAULT_GROUP");
			// find groupe && assign USER/GROUPE in table : ACM_USERS_GROUPE
			groupeDTOs = parametrageClient.find(paramsGroupeDTO);
		}
		// processing data and assign user to group
		if (!ACMValidationUtils.isNullOrEmpty(groupeDTOs)) {
			groupeDTOs.forEach(groupeDTO -> {
				Groupe groupe = mapper.map(groupeDTO, Groupe.class);
				groupe.setInsertBy("ADMIN");
				groupe.setAcmVersion(0);
				groupe.setDateInsertion(new Date());
				groupe.setEnabled(Boolean.TRUE);
				if (ACMValidationUtils.isNullOrEmpty(user.getGroupes())) {
					Set<Groupe> groupesFounded = new HashSet<>();
					groupesFounded.add(groupe);
					user.setGroupes(groupesFounded);
				}
				else {
					user.getGroupes().add(groupe);
				}
			});
		}
		logger.info("Executing Method assignGroupeUser() : {} :: DONE", user.getGroupes());
		return user;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#findUsers(java.lang.Boolean)
	 */
	@Override
	public List<UserDTO> findUsers(Boolean fullList) {

		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		UserDTO connectedUser = (UserDTO) authentication.getPrincipal();

		connectedUser = find(connectedUser.getLogin());
		// setting type to CONNECTED_USER
		connectedUser.setTypeUser(UserHierarchicalType.CONNECTED_USER.name());

		List<UserDTO> userDTOs = new ArrayList<>();
		// Connected user
		userDTOs.add(connectedUser);

		// init list RESPONSIBLE list
		List<User> responsables =
				findResponsables(connectedUser.getResponsableId(), MAX_DEEP_SEARCH_RESPONSABLE);
		responsables.forEach(user -> {
			UserDTO userDTO = mapper.map(user, UserDTO.class);
			// setting RESPONSIBLE
			userDTO.setTypeUser(UserHierarchicalType.SUPERVISOR.name());
			userDTOs.add(userDTO);
		});

		// init list COLLABORATORS
		List<UserDTO> collaborators = findCollaborators(connectedUser.getLogin());
		userDTOs.addAll(collaborators);

		// fullList=TRUE : if method called when "assign application" action
		if (Boolean.TRUE.equals(fullList)) {
			// find user with category = MANAGMENT && same branch of connected user
			List<User> usersManagment =
					userRepository.findByCategoryAndAccessBranchesContainingAndEnabled(
							UserCategory.MANAGMENT.name(), connectedUser.getBranchID().toString(),
							Boolean.TRUE);
			logger.info("{} : Users with category : MANAGMENT and BRANCH : {} was founded",
					usersManagment.size(), connectedUser.getBranchDescription());
			if (!ACMValidationUtils.isNullOrEmpty(usersManagment)) {
				usersManagment.forEach(
						userManagment -> userDTOs.add(mapper.map(userManagment, UserDTO.class)));
			}

			// find loan by Access Branches for connected user with type = MANAGMENT
			if (connectedUser.getCategory() != null
					&& connectedUser.getCategory().equals(UserCategory.MANAGMENT.name())
					&& !ACMValidationUtils.isNullOrEmpty(connectedUser.getAccessBranches())) {
				int[] arrayBranchIds = Arrays.asList(connectedUser.getAccessBranches().split(","))
						.stream().map(String::trim).mapToInt(Integer::parseInt).toArray();
				logger.info("ID Access Branches = {}", arrayBranchIds);
				List<Integer> listBranchIds = new ArrayList<>(arrayBranchIds.length);
				for (int i : arrayBranchIds) {
					listBranchIds.add(Integer.valueOf(i));
				}
				// find all users in Access Branches and not CUSTOMER
				List<User> usersByAccessBranches =
						userRepository.findByBranchIDInAndEnabledAndCategoryNot(listBranchIds,
								Boolean.TRUE, UserCategory.CUSTOMER.name());
				logger.info("{} : Users with category : MANAGMENT and by BRANCHES : {} was founded",
						usersByAccessBranches.size(), arrayBranchIds);
				// mapping data
				if (!ACMValidationUtils.isNullOrEmpty(usersByAccessBranches)) {
					usersByAccessBranches
							.forEach(user -> userDTOs.add(mapper.map(user, UserDTO.class)));
				}
			}
		}

		// Filter & Remove duplicate User
		List<UserDTO> userDTOFiltered =
				userDTOs.stream().filter(CommonFunctions.distinctByKey(UserDTO::getLogin))
						.collect(Collectors.toList());

		// Remove TECHNICAL users
		userDTOFiltered.removeIf(user -> user.getLogin().equals(CommonConstants.DEFAULT_USER));

		logger.info("find Users : {} : User was founded", userDTOFiltered.size());
		return userDTOFiltered;
	}

	/**
	 * recursive function to Find collaborators by given responsableId.
	 *
	 * @author HaythemBenizid
	 * @param responsableId the responsable id
	 * @return the list
	 */
	public  List<UserDTO> findCollaborators(String responsableId) {

		List<UserDTO> userDTOs = new ArrayList<>();
		List<User> collaborators = userRepository.findByResponsableId(responsableId);
		if (!ACMValidationUtils.isNullOrEmpty(collaborators)) {
			collaborators.forEach(user -> {
				UserDTO userDTO = mapper.map(user, UserDTO.class);
				// setting COLLABORATORS
				userDTO.setTypeUser(UserHierarchicalType.COLLABORATORS.name());
				userDTOs.add(userDTO);
				// if user has COLLABORATORS
				userDTOs.addAll(findCollaborators(userDTO.getLogin()));
			});
		}
		logger.debug("Executing Method findCollaborators() :: DONE");
		return userDTOs;
	}

	/**
	 * recursive function to Find responsables by given login.
	 *
	 * @author HaythemBenizid
	 * @param login the login
	 * @param deep the deep
	 * @return the list
	 */
	private List<User> findResponsables(String login, int deep) {

		List<User> userDTOs = new ArrayList<>();
		User responsable = userRepository.findByUsernameIgnoreCase(login);
		if (responsable != null) {
			userDTOs.add(responsable);
			if (!responsable.getResponsableId().equals("0") && deep > 0) {
				userDTOs.addAll(findResponsables(responsable.getResponsableId(), deep--));
			}
			else {
				return userDTOs;
			}
		}
		logger.debug("Executing Method findResponsables() :: DONE");
		return userDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#find()
	 */
	@Override
	public List<UserDTO> find() {

		List<User> users = userRepository.findAll();
		List<UserDTO> usersDTOs = new ArrayList<>();
		users.forEach(user -> usersDTOs.add(mapper.map(user, UserDTO.class)));
		logger.debug("Executing Method find ALL :: DONE");
		return usersDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#updatePWD(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO updatePWD(UserDTO userDTO)
			throws PwdConfirmInvalidException, OldPwdInvalidException {

		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(userDTO.getLogin(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(userDTO.getPwd(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(userDTO.getPwdConfirm(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(userDTO.getPwdNew(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check new pwd
		if (!userDTO.getPwdConfirm().equals(userDTO.getPwdNew())) {
			throw new PwdConfirmInvalidException(
					new ExceptionResponseMessage(CommonErrorCode.PWD_DONT_MATCH));
		}
		// find user by login
		User updatedUser = userRepository.findByUsernameIgnoreCase(userDTO.getLogin());

		Boolean validPWD =
				cryptPasswordEncoder.matches(userDTO.getPwd().trim(), updatedUser.getPassword());
		if (Boolean.FALSE.equals(validPWD)) {
			throw new OldPwdInvalidException(
					new ExceptionResponseMessage(CommonErrorCode.PWD_INVALID));
		}
		// setting encrypted pwd
		updatedUser.setPassword(cryptPasswordEncoder.encode(userDTO.getPwdNew()));
		updatedUser.setPwdExpiryDate(DateUtil.addDays(new Date(), getPwdExpireDelay()));

		updatedUser.setTemporaryPwd(Boolean.FALSE);
		/// Update User
		User user = userRepository.save(updatedUser);

		logger.info("Executing Method updatePWD() :: DONE");
		return mapper.map(user, UserDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#saveEnable(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO saveEnable(UserDTO userDTO) {

		// Check userDTO not null
		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Check Login not null
		Preconditions.checkNotNull(userDTO.getLogin(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Get Old User by login
		User oldUser = userRepository.findByUsernameIgnoreCase(userDTO.getLogin());

		boolean resetPWD = false;
		if (oldUser.getEnabled().equals(Boolean.FALSE) && userDTO.getEnabled().equals(Boolean.TRUE)
				&& oldUser.getFailedAttempts() != null && oldUser.getFailedAttempts() > 0) {
			oldUser.setFailedAttempts(0);
			resetPWD = true;
		}
		// Change User enable
		oldUser.setEnabled(userDTO.getEnabled());
		// Update User
		User user = userRepository.save(oldUser);

		if (resetPWD) {
			try {
				resetPWD(userDTO.getLogin());
			}
			catch (ResetPwdException e) {
				logger.error("Failed to reset PWD when activating user !");
			}
		}

		logger.info("Executing Method saveEnable() :: DONE");
		return mapper.map(user, UserDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findGuarantor(com.acm.utils.dtos.pagination.
	 * CustomerPaginationDTO)
	 */
	@Override
	public UserPaginationDTO find(UserPaginationDTO userPaginationDTO) {

		Preconditions.checkNotNull(userPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getPageNumber())) {
			userPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getPageSize())) {
			userPaginationDTO.setPageSize(10);
		}
		// setting default data
		userPaginationDTO.setResultsUsers(new ArrayList<>());
		// setting default totals pages
		userPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		userPaginationDTO.setTotalPages(0);
		// init QUser
		QUser qUser = QUser.user;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only not CUSTOMER user
		predicate.and(qUser.category.ne(UserCategory.CUSTOMER.name()));
		// find only not acm batch
		predicate.and(qUser.username.ne("acmbatch"));
		// find only not super admin
		predicate.and(qUser.username.ne(CommonConstants.DEFAULT_USER));

		// find LIKE UserLogin
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getLogin())) {
			predicate
					.and(qUser.username.like("%" + userPaginationDTO.getParams().getLogin() + "%"));
		}
		// find LIKE fullName
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getPortfolioName())) {
			predicate
					.and(qUser.nom
							.like("%" + userPaginationDTO.getParams().getPortfolioName() + "%"))
					.or(qUser.prenom
							.like("%" + userPaginationDTO.getParams().getPortfolioName() + "%"));
		}
		// find LIKE Name
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getNom())) {
			predicate.and(qUser.nom.like("%" + userPaginationDTO.getParams().getNom() + "%"));
		}

		// find LIKE Prenom
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getPrenom())) {
			predicate.and(qUser.prenom.like("%" + userPaginationDTO.getParams().getPrenom() + "%"));
		}

		// find LIKE Mail
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getEmail())) {
			predicate.and(qUser.email.like("%" + userPaginationDTO.getParams().getEmail() + "%"));
		}

		// find LIKE Branch
		if (!ACMValidationUtils
				.isNullOrEmpty(userPaginationDTO.getParams().getBranchDescription())) {
			predicate.and(qUser.branchDescription
					.like("%" + userPaginationDTO.getParams().getBranchDescription() + "%"));
		}

		// find LIKE Responsible
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getResponsableId())) {
			predicate.and(qUser.responsableId
					.like("%" + userPaginationDTO.getParams().getResponsableId() + "%"));
		}
		// find by enabled
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getEnabled())) {
			predicate.and(qUser.enabled.eq(userPaginationDTO.getParams().getEnabled()));
		}

		// find by enabled
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getEnabled())) {
			predicate.and(qUser.enabled.eq(userPaginationDTO.getParams().getEnabled()));
		}

		// find by group ID (Join with Table : ACM_USERS_GROUPE )
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getGroupes())) {
			// get the first item from Set
			GroupeDTO groupeDTO =
					userPaginationDTO.getParams().getGroupes().stream().findFirst().orElse(null);
			Long idGroupe =
					(groupeDTO != null && groupeDTO.getId() != null) ? groupeDTO.getId() : 0L;
			if (idGroupe != 0) {
				predicate.and(qUser.groupes.any().id.eq(idGroupe));
			}
		}
		// find by temporaryResponsable
		if (!ACMValidationUtils
				.isNullOrEmpty(userPaginationDTO.getParams().getTemporaryResponsable())) {
			predicate.and(qUser.temporaryResponsable
					.eq(userPaginationDTO.getParams().getTemporaryResponsable()));
		}
		// find by oldResponsibleName
		if (!ACMValidationUtils
				.isNullOrEmpty(userPaginationDTO.getParams().getOldResponsableName())) {
			predicate.and(qUser.oldResponsableName
					.like("%" + userPaginationDTO.getParams().getOldResponsableName() + "%"));
		}
		// find LIKE Employee Id
		if (!ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getParams().getEmployeeId())) {
			predicate.and(qUser.employeeId
					.like("%" + userPaginationDTO.getParams().getEmployeeId() + "%"));
		}
		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(userPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getSortField())) {
			String sortedField = userPaginationDTO.getSortField();
			if (userPaginationDTO.getSortField().equals("login")) {
				sortedField = "username";
			}
			if (userPaginationDTO.getSortField().equals("groupes")) {
				sortedField = "groupes.code";
			}
			pageable = PageRequest.of(userPaginationDTO.getPageNumber(),
					userPaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
		}
		else if ("-1".equals(userPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(userPaginationDTO.getSortField())) {
			String sortedField = userPaginationDTO.getSortField();
			if (userPaginationDTO.getSortField().equals("login")) {
				sortedField = "username";
			}
			if (userPaginationDTO.getSortField().equals("groupes")) {
				sortedField = "groupes.code";
			}
			pageable = PageRequest.of(userPaginationDTO.getPageNumber(),
					userPaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
		}
		else {
			// default sort by customerName : DESC
			pageable = PageRequest.of(userPaginationDTO.getPageNumber(),
					userPaginationDTO.getPageSize(), Sort.Direction.ASC, "username");
		}
		// load data
		Page<User> pagedResult = userRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<User> users = pagedResult.getContent();
			logger.info("{} : User was founded (PageNumber = {} / PageSize = {} )", users.size(),
					userPaginationDTO.getPageNumber(), userPaginationDTO.getPageSize());
			List<UserDTO> userDTOs = new ArrayList<>();
			users.forEach(user -> userDTOs.add(mapper.map(user, UserDTO.class)));
			// setting data
			userPaginationDTO.setResultsUsers(userDTOs);
			// setting totals pages
			userPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			userPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		logger.info("Executing Method UserPagination() :: DONE");
		return userPaginationDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#save(java.lang.String, com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO save(String login, UserDTO userDTO) throws CustomException {

		// Check userDTO not null
		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Check Login not null
		Preconditions.checkNotNull(login, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Get Old User by login
		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		UserDTO connectedUser = (UserDTO) authentication.getPrincipal();

		User oldUser = userRepository.findByUsernameIgnoreCase(login);
		oldUser.setNom(userDTO.getNom());
		oldUser.setPrenom(userDTO.getPrenom());
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getGroupes())) {
			oldUser.setGroupes(new HashSet<>());
			oldUser.getGroupes()
					.add(mapper.map(userDTO.getGroupes().iterator().next(), Groupe.class));
		}
		oldUser.setCategory(userDTO.getCategory());
		oldUser.setResponsableId(userDTO.getResponsableId().equals(oldUser.getUsername()) ? "0"
				: userDTO.getResponsableId());
		oldUser.setAccountPortfolioId(
				userDTO.getAccountPortfolioId() != null ? userDTO.getAccountPortfolioId() : 0);
		oldUser.setEmail(userDTO.getEmail());
		oldUser.setBranchID(userDTO.getBranchID());
		oldUser.setBranchName(userDTO.getBranchName());
		oldUser.setBranchDescription(userDTO.getBranchDescription());
		oldUser.setAccessBranches(userDTO.getAccessBranches());
		oldUser.setPortfolioName(
				userDTO.getPortfolioName() != null ? userDTO.getPortfolioName() : "");
		oldUser.setResigningDate(userDTO.getResigningDate());
		oldUser.setHiringDate(userDTO.getHiringDate());
		oldUser.setEmployeeId(userDTO.getEmployeeId());
		oldUser.setUpdatedBy(connectedUser.getLogin());
		oldUser.setDateLastUpdate(new Date());
		oldUser.setAcmVersion(oldUser.getAcmVersion() + 1);
		// Update User
		try {
			User user = userRepository.save(oldUser);

			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, User.class.getSimpleName());
			// change the responsilble id for all users with the same old responsible iff the
			// functionnality is activated
			if (Boolean.TRUE.equals(userDTO.getChangeAllResponsible())
					&& !ACMValidationUtils.isNullOrEmpty(userDTO.getOldResponsibleId())) {
				updateUsersResponsible(userDTO);
			}
			return mapper.map(user, UserDTO.class);
		}
		catch (DataIntegrityViolationException e) {
			if (e.getCause().getCause().toString().contains("EMPLOYEE_ID")) {
				logger.error("Save User Exception : EMPLOYEE_ID duplicated ");
				throw new CustomException(CommonErrorCode.CODE_USER_EXIST);
			}
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#findUserByBranchId(java.util.List)
	 */
	@Override
	public List<UserDTO> findUsersByBranchId() {

		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		UserDTO connectedUser = find(authentication.getName());
		// setting type to CONNECTED_USER
		connectedUser.setTypeUser(UserHierarchicalType.CONNECTED_USER.name());
		List<UserDTO> userDTOs = new ArrayList<>();
		// Connected user
		userDTOs.add(connectedUser);
		// init list Users by branch id list
		logger.info("User connected branch = {}", connectedUser.getBranchName());
		List<User> usersBranch = userRepository.findByBranchID(connectedUser.getBranchID());
		usersBranch.forEach(user -> {
			UserDTO userDTO = mapper.map(user, UserDTO.class);
			userDTOs.add(userDTO);
		});
		logger.info("findUsersByBranchId : {} : Users was founded", userDTOs.size());
		return userDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#findPortfolio(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public List<UserDTO> findPortfolio(UserDTO userDTO) {

		// init QUser
		QUser qUser = QUser.user;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		predicate.and(qUser.enabled.eq(Boolean.TRUE));
		predicate.and(qUser.accountPortfolioId.ne(0L));

		// find by branch id
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getBranchID())) {
			predicate.and(qUser.branchID.eq(userDTO.getBranchID()));
		}

		Iterable<User> iterable = userRepository.findAll(predicate);
		List<User> users = new ArrayList<>();
		iterable.forEach(users::add);
		logger.info("findPortfolio : {} : Users was founded", users.size());

		// mapping data
		List<UserDTO> userDTOs = new ArrayList<>();
		users.forEach(user -> userDTOs.add(mapper.map(user, UserDTO.class)));
		logger.debug("Executing Method findPortfolio() :: DONE");
		return userDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#findByGroupe(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public List<UserDTO> findByGroupe(UserDTO userDTO) {

		// Check userDTO not null
		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getGroupeCode())) {
			// find groupe by code
			List<GroupeDTO> groupeDTOs =
					parametrageClient.find(new GroupeDTO(userDTO.getGroupeCode()));
			// find users by ID GROUPE
			if (!ACMValidationUtils.isNullOrEmpty(groupeDTOs)) {
				List<User> users = userRepository.findByGroupe(groupeDTOs.get(0).getId());
				logger.info("findByGroupe : {} : Users was founded", users.size());
				// mapping data
				List<UserDTO> usersDTOs = new ArrayList<>();
				users.forEach(user -> usersDTOs.add(mapper.map(user, UserDTO.class)));
				return usersDTOs;
			}
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#resetPWD(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO resetPWD(String login) throws ResetPwdException {

		// Check Login not null
		Preconditions.checkNotNull(login, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check if user exist by login
		User oldUser = userRepository.findByUsernameIgnoreCase(login);
		if (ACMValidationUtils.isNullOrEmpty(oldUser)) {
			logger.warn("### User with User_name : {} is not exists in DB ###", login);
			throw new ResetPwdException(
					new ExceptionResponseMessage(CommonErrorCode.LOGIN_INVALID,
							CommonExceptionsMessage.LOGIN_INVALID, new TechnicalException()),
					CommonExceptionsMessage.LOGIN_INVALID);
		}
		else {
			// generate new random PWD
			String pwd = CommonFunctions.generateRandomString(8);
			logger.info("New PWD = {}", pwd);
			// setting encrypted pwd
			oldUser.setPassword(cryptPasswordEncoder.encode(pwd));
			oldUser.setPwdExpiryDate(DateUtil.addDays(new Date(), getPwdExpireDelay()));
			oldUser.setTemporaryPwd(Boolean.TRUE);
			// Update User
			User user = userRepository.save(oldUser);

			UserDTO updatedUserDTO = mapper.map(user, UserDTO.class);
			updatedUserDTO.setPwd(pwd);
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, User.class.getSimpleName());

			// send notification mail to customer member
			sendMail(new MailCustomerDTO(updatedUserDTO, updatedUserDTO.getLogin(),
					updatedUserDTO.getPwd(),
					new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
							(!ACMValidationUtils.isNullOrEmpty(updatedUserDTO.getEmail())
									&& Boolean.TRUE.equals(
											StringUtils.mailIsValid(updatedUserDTO.getEmail())))
													? updatedUserDTO.getEmail()
													: defaultACMReceiverMail,
							"Reset password", ""),
					MailBuilderMethod.BUILD_CUSTOMER_RESET_PWD));
			return updatedUserDTO;
		}
	}

	/**
	 * Send mail.
	 *
	 * @author MoezMhiri
	 * @param mailCustomerDTO the mail customer DTO
	 */
	private void sendMail(MailCustomerDTO mailCustomerDTO) {

		try {
			mailSenderClient.sendEmail(mailCustomerDTO);
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
	 * @see com.acm.service.UserService#saveDefaultLang(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO saveDefaultLang(UserDTO userDTO) {

		// Check userDTO not null
		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Check Login not null
		Preconditions.checkNotNull(userDTO.getLogin(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Check DefaultLang not null
		Preconditions.checkNotNull(userDTO.getDefaultLang(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Get Old User by login
		User oldUser = userRepository.findByUsernameIgnoreCase(userDTO.getLogin());
		// Change User enable
		oldUser.setDefaultLang(userDTO.getDefaultLang());
		// Update User
		User user = userRepository.save(oldUser);

		logger.info("Executing Method saveDefaultLang() :: DONE");
		return mapper.map(user, UserDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#findByGroupeCodeAndBranchID(java.lang.String,
	 * java.lang.Integer)
	 */
	@Override
	public List<UserDTO> findByGroupeCodeAndBranchID(String codeGroupe, Integer branchId) {

		// Check data not null
		if (ACMValidationUtils.isNullOrEmpty(codeGroupe)
				|| ACMValidationUtils.isNullOrEmpty(branchId)) {
			return new ArrayList<>();
		}

		// find data ByGroupeCode And BranchID
		List<User> users = userRepository.findByGroupeCodeAndBranchID(codeGroupe, branchId);

		// mapping data
		List<UserDTO> userDTOs = new ArrayList<>();
		users.forEach(user -> userDTOs.add(mapper.map(user, UserDTO.class)));

		logger.info("Executing Method findByGroupeCodeAndBranchID() :: DONE");
		return userDTOs;
	}

	/**
	 * Update users responsible.
	 * 
	 * @author idridi
	 * @param userDTO the user DTO
	 */
	private void updateUsersResponsible(UserDTO userDTO) {

		// Check userDTO not null
		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		if (ACMValidationUtils.isNullOrEmpty(userDTO.getResponsableId())
				|| ACMValidationUtils.isNullOrEmpty(userDTO.getOldResponsibleId())) {
			logger.error("old responsible Id or responsible is empty");
		}
		else {
			// find list of users by responsibleId
			List<User> users = userRepository.findByResponsableId(userDTO.getOldResponsibleId());

			// update users responsibleId
			for (User user : users) {
				// set new responsible Id
				user.setResponsableId(userDTO.getResponsableId());
				// Update User
				User newUser = userRepository.save(user);
				logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, newUser);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.UserService#findByGroupeCodeAndBranchIDAndAccessBranches(java.lang.String,
	 * java.lang.Integer)
	 */
	@Override
	public List<UserDTO> findByGroupeCodeAndBranchIDAndAccessBranches(String codeGroupe,
			Integer branchId) {

		// Check data not null
		if (ACMValidationUtils.isNullOrEmpty(codeGroupe)
				&& ACMValidationUtils.isNullOrEmpty(branchId)) {
			return new ArrayList<>();
		}
		// find branchID in access branch
		String branchIdRight = "%," + branchId;
		String branchIdMiddle = "%," + branchId + ",%";
		String branchIdLeft = branchId + ",%";

		// find data ByGroupeCode And BranchID
		List<User> users = userRepository.findByGroupeCodeAndBranchIdAndAccessBranches(codeGroupe,
				branchId, branchIdRight, branchIdMiddle, branchIdLeft);

		// mapping data
		List<UserDTO> userDTOs = new ArrayList<>();
		users.forEach(user -> userDTOs.add(mapper.map(user, UserDTO.class)));

		logger.info("Executing Method findByGroupeCodeAndBranchIdAndAccessBranches() :: DONE");
		return userDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#getUsersWithLoanBranchInTheirAccessBranches(java.util.List,
	 * java.lang.Integer)
	 */
	@Override
	public List<UserDTO> getUsersWithLoanBranchInTheirAccessBranches(List<UserDTO> usersDTO,
			Integer loanBranchId) {

		// Check userDTOs not null
		Preconditions.checkNotNull(usersDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// default owner
		String owner = CommonConstants.DEFAULT_USER;
		// list of users that doesn't have loanBranch in their accessBranch
		List<UserDTO> usersToRemove = new ArrayList<>();
		// remove users that don't have branchLoan in their accessBranches
		if (!ACMValidationUtils.isNullOrEmpty(usersDTO)) {
			usersDTO.forEach(userDTOParam -> {
				if (!ACMValidationUtils.isNullOrEmpty(userDTOParam.getAccessBranches())) {
					List<Integer> arrayBranchIds =
							Arrays.asList(userDTOParam.getAccessBranches().split(",")).stream()
									.map(String::trim).mapToInt(Integer::parseInt)
									.collect(ArrayList::new, ArrayList::add, ArrayList::addAll);
					logger.info("ID Access Branches = {}", arrayBranchIds);
					// if userDTOParam doesn't have loanBranch in his accessBranches then add it to
					// list of usersToRemove
					// if loanBranchId is null then the loan will be assigned to all the group users
					if (!ACMValidationUtils.isNullOrEmpty(loanBranchId)) {
						if (!arrayBranchIds.contains(loanBranchId)
								|| Boolean.FALSE.equals(userDTOParam.getEnabled())) {
							usersToRemove.add(userDTOParam);
						}
					}
				}
			});
		}
		// remove usersToRemove
		if (usersToRemove.size() != 0) {
			usersDTO.removeAll(usersToRemove);
		}
		// if the list is empty then add super admin to the list
		if (usersDTO.size() == 0) {
			usersDTO.add(find(owner));
		}
		// return only users that have loanBranch in their accessBranches OR if the list is empty,
		// then return the superAdmin
		return usersDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#getResponsibleOfUser(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO findResponsibleOfUser(UserDTO userDTO) {

		// Check userDTO not null
		Preconditions.checkNotNull(userDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// Check portfolioId and Username of user not null

		if (ACMValidationUtils.isNullOrEmpty(userDTO.getAccountPortfolioId())
				&& ACMValidationUtils.isNullOrEmpty(userDTO.getLogin())) {
			// TODO throw Exception
		}

		// get users by portfolio ID
		List<UserDTO> userDtos = find(userDTO);
		// get responsible of the portfolio
		if (!ACMValidationUtils.isNullOrEmpty(userDtos)) {
			User responsible =
					userRepository.findByUsernameIgnoreCase(userDtos.get(0).getResponsableId());
			logger.info("user founded : {}", responsible);
			return mapper.map(responsible, UserDTO.class);
		}
		// return connected user
		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		UserDTO connectedUser = (UserDTO) authentication.getPrincipal();
		User user = userRepository.findByUsernameIgnoreCase(connectedUser.getLogin());
		logger.info("connected user founded : {}", user);
		return mapper.map(user, UserDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#updateUsersResponsible(java.util.List)
	 */
	@Override
	public List<UserDTO> updateUsersResponsible(List<UserDTO> usersDTOs) {

		List<UserDTO> updatedUsersDTOs = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(usersDTOs)) {
			for (UserDTO userDTO : usersDTOs) {
				Preconditions.checkNotNull(userDTO.getLogin(),
						CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
				// get old user
				User oldUser = userRepository.findByUsernameIgnoreCase(userDTO.getLogin());
				logger.info("user founded : {}", oldUser);
				if (!ACMValidationUtils.isNullOrEmpty(oldUser)) {
					// set temporary value
					oldUser.setTemporaryResponsable(userDTO.getTemporaryResponsable());
					// set the new responsible id
					oldUser.setResponsableId(userDTO.getResponsableId());
					// find old responsible
					UserDTO oldResponsibleDTO = find(userDTO.getOldResponsibleId());
					// set old responsable name and id
					oldUser.setOldResponsableName(oldResponsibleDTO.getFullName());
					oldUser.setOldResponsibleId(userDTO.getOldResponsibleId());
					// Update User
					User user = userRepository.save(oldUser);
					logger.info("user updated with new supervisor : {}", user);
					// mapping data
					updatedUsersDTOs.add(mapper.map(user, UserDTO.class));
				}

			}
		}
		return updatedUsersDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#findResponsible(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public List<UserDTO> findResponsible(UserDTO userDTO) {

		// init QUser
		QUser qUser = QUser.user;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only not CUSTOMER user
		predicate.and(qUser.category.ne(UserCategory.CUSTOMER.name()));
		// find only not acm batch
		predicate.and(qUser.username.ne("acmbatch"));
		// find only not super admin
		predicate.and(qUser.username.ne(CommonConstants.DEFAULT_USER));
		// find by branch id and 1 (Head Office) users by given branchid
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getBranchID())) {
			predicate.and(qUser.branchID.in(userDTO.getBranchID(), 1));
		}
		// find by group ID (Join with Table : ACM_USERS_GROUPE )
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getGroupes())) {
			// get the first item from Set
			GroupeDTO groupeDTO = userDTO.getGroupes().stream().findFirst().orElse(null);
			Long idGroupe =
					(groupeDTO != null && groupeDTO.getId() != null) ? groupeDTO.getId() : 0L;
			if (idGroupe != 0) {
				predicate.and(qUser.groupes.any().id.eq(idGroupe));
			}
		}
		Iterable<User> iterable = userRepository.findAll(predicate);
		List<User> users = new ArrayList<>();
		iterable.forEach(users::add);
		logger.info("{} : User was founded", users.size());

		// mapping data
		List<UserDTO> userDTOs = new ArrayList<>();
		users.forEach(user -> userDTOs.add(mapper.map(user, UserDTO.class)));
		return userDTOs;
	}

	/**
	 * Gets the pwd expire delay.
	 *
	 * @return the pwd expire delay
	 */
	public Integer getPwdExpireDelay() {

		if (pwdExpireDelay == null) {
			try {
				AcmEnvironnementDTO acmEnvironement = parametrageClient.find(USER_PWD_EXPIRE_DELAY);
				if (!ACMValidationUtils.isNullOrEmpty(acmEnvironement)
						&& ACMValidationUtils.isNumeric(acmEnvironement.getValue())) {
					pwdExpireDelay = Integer.valueOf(acmEnvironement.getValue().trim());
				}
			}
			catch (Exception e) {
				logger.error("Cannont find User password expiry delay !");
			}
			if (pwdExpireDelay == null) {
				pwdExpireDelay = DEFAULT_PWD_EXPIRE_DELAY; // Default delay
			}
		}
		return pwdExpireDelay;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserService#countUser()
	 */
	@Override
	public Integer countUser() {

		return userRepository.countByEnabled(true);
	}

}
