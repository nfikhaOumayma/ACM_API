/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.security.Principal;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLicenceVariable;
import com.acm.exceptions.type.CustomException;
import com.acm.exceptions.type.OldPwdInvalidException;
import com.acm.exceptions.type.PwdConfirmInvalidException;
import com.acm.exceptions.type.ResetPwdException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.UserService;
import com.acm.service.impl.UserServiceImpl;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.UserPaginationDTO;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;

/**
 * The {@link UserController} class used to control all the User request .
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RestController
@RequestMapping("/users")
public class UserController {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(UserController.class);

	/** The user service. */
	@Autowired
	private UserService userService;
	
	@Autowired
	private UserServiceImpl userServiceimpl;

	/**
	 * Find connected user, return Principal used in security configuration with other microservice.
	 * 
	 * @author HaythemBenizid
	 * @param user the user
	 * @return the principal
	 */
	@GetMapping("/principal")
	public Principal findUserPrincipal(Principal user) {

		return user;
	}
	
	
	
	 @GetMapping("/getCollaborators")
	    public List<UserDTO> getCollaborators(@RequestHeader("responsibleId") String responsibleId) {
	        return userServiceimpl.findCollaborators(responsibleId);
	    }
	

	/**
	 * Find user by login.
	 *
	 * @author HaythemBenizid
	 * @param login the login
	 * @return the user DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{login}")
	public UserDTO findByLogin(@PathVariable("login") String login)
			throws ResourcesNotFoundException {

		return userService.find(login);
	}

	/**
	 * The method used for finding the connected user.
	 * 
	 * @author HaythemBenizid
	 * @param principal the principal
	 * @return {@link UserDTO}
	 */
	@GetMapping(value = "/connected")
	public UserDTO find(Principal principal) {

		return CommonFunctions.getConnectedUser(logger);
	}

	/**
	 * Find list user by given params.
	 * 
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<UserDTO> find(@RequestBody UserDTO userDTO) {

		return userService.find(userDTO);
	}

	/**
	 * Find responsible.
	 * 
	 * @author idridi
	 * @param userDTO the user DTO
	 * @return the list
	 */
	@PostMapping("/find-responsible")
	public List<UserDTO> findResponsible(@RequestBody UserDTO userDTO) {

		return userService.findResponsible(userDTO);
	}

	/**
	 * Create the given user.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 * @throws CustomException the custom exception
	 */
	@PostMapping("/create")
	public UserDTO create(@RequestBody UserDTO userDTO) throws CustomException {

		return userService.save(userDTO);
	}

	/**
	 * Create the given IB user.
	 * 
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	@PostMapping("/create-for-ib")
	public UserDTO createForIB(@RequestBody UserDTO userDTO) {

		return userService.saveForIB(userDTO);
	}

	/**
	 * Create the given user (USED ONLY BY BATCH).
	 * 
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	@PostMapping("/create-for-batch")
	public UserDTO createForBatch(@RequestBody UserDTO userDTO) {

		return userService.saveForBatch(userDTO);
	}

	/**
	 * Find list user (ALL {@link UserHierarchicalType}) for connected user.
	 * 
	 * @author MoezMhiri
	 * @return the list
	 */
	@GetMapping("/findUsers")
	public List<UserDTO> findUsers() {

		return userService.findUsers(Boolean.FALSE);
	}

	/**
	 * Find list user (ALL {@link UserHierarchicalType}) for connected user && if FullList=True add
	 * list of user with {@link UserCategory}=MANAGMENT for his branch.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-full-list")
	public List<UserDTO> findFullListUsers() {

		return userService.findUsers(Boolean.TRUE);
	}

	/**
	 * Load USER category type.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/user-category")
	public List<AcmStatutsDTO> loadCategoryType() {

		List<AcmStatutsDTO> acmStatutsDTOs = new ArrayList<>();
		acmStatutsDTOs.add(new AcmStatutsDTO(1, UserCategory.OPERATION.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(2, UserCategory.MANAGMENT.name()));
		return acmStatutsDTOs;
	}

	/**
	 * Update Only Enable of User by UserDTO.
	 *
	 * @author YesserSomai
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	@PostMapping("/enable")
	public UserDTO saveEnable(@RequestBody UserDTO userDTO) {

		return userService.saveEnable(userDTO);
	}

	/**
	 * Update of User by UserDTO.
	 *
	 * @author YesserSomai
	 * @param userDTO the user DTO
	 * @return the user DTO
	 * @throws CustomException the custom exception
	 */
	@PostMapping("/update")
	public UserDTO save(@RequestBody UserDTO userDTO) throws CustomException {

		return userService.save(userDTO.getLogin(), userDTO);
	}

	/**
	 * Find list user by given branch id.
	 * 
	 * @author MoezMhiri
	 * @return the list
	 */
	@GetMapping("/findUsersBranch")
	public List<UserDTO> findUsersBranch() {

		return userService.findUsersByBranchId();
	}

	/**
	 * Find all portfolio.
	 * 
	 * @author AbdelkarimTurki
	 * @param userDTO the user DTO
	 * @return the list
	 */
	@PostMapping("/find-portfolio")
	public List<UserDTO> findPortfolio(@RequestBody UserDTO userDTO) {

		return userService.findPortfolio(userDTO);
	}

	/**
	 * Find user by given by groupe Code.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the list
	 */
	@PostMapping("/find-by-groupe")
	public List<UserDTO> findByGroupe(@RequestBody UserDTO userDTO) {

		return userService.findByGroupe(userDTO);
	}

	/**
	 * Find by groupe code and branch ID.
	 *
	 * @author Yesser Somai
	 * @param codeGroupe the code groupe
	 * @param branchId the branch id
	 * @return the list
	 */
	@GetMapping("/find-by-groupe-branch/{codeGroupe}/{branchId}")
	public List<UserDTO> findByGroupeCodeAndBranchID(@PathVariable String codeGroupe,
			@PathVariable Integer branchId) {

		return userService.findByGroupeCodeAndBranchID(codeGroupe, branchId);
	}

	/**
	 * Forget pwd the given user.
	 *
	 * @author MoezMhiri
	 * @param userDTO the user DTO
	 * @return the user DTO
	 * @throws ResetPwdException the reset pwd exception
	 */
	@PostMapping("/forget-pwd")
	public UserDTO resetPwd(@RequestBody UserDTO userDTO) throws ResetPwdException {

		return userService.resetPWD(userDTO.getLogin());
	}

	/**
	 * Update pwd the given user.
	 * 
	 * @author MoezMhiri
	 * @param userDTO the user DTO
	 * @return the user DTO
	 * @throws PwdConfirmInvalidException the pwd confirm invalid exception
	 * @throws OldPwdInvalidException the old pwd invalid exception
	 */
	@PostMapping("/update-pwd")
	public UserDTO updatePwd(@RequestBody UserDTO userDTO)
			throws PwdConfirmInvalidException, OldPwdInvalidException {

		return userService.updatePWD(userDTO);
	}

	/**
	 * Find pagination.
	 * 
	 * @author Salmen Fatnassi
	 * @param userPaginationDTO the user pagination DTO
	 * @return the user pagination DTO
	 */
	@PostMapping("/find-pagination")
	public UserPaginationDTO findUsersPagination(@RequestBody UserPaginationDTO userPaginationDTO) {

		return userService.find(userPaginationDTO);
	}

	/**
	 * Update Only DefaultLang of given User.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	@PostMapping("/set-default-lang")
	public UserDTO saveDefaultLang(@RequestBody UserDTO userDTO) {

		return userService.saveDefaultLang(userDTO);
	}

	/**
	 * Find by groupe code and branch ID and access branches.
	 * 
	 * @author idridi
	 * @param codeGroupe the code groupe
	 * @param branchId the branch id
	 * @return the list
	 */
	@GetMapping("/find-by-groupe-branch-access-branches/{codeGroupe}/{branchId}")
	public List<UserDTO> findByGroupeCodeAndBranchIDAndAccessBranches(
			@PathVariable String codeGroupe, @PathVariable Integer branchId) {

		return userService.findByGroupeCodeAndBranchIDAndAccessBranches(codeGroupe, branchId);
	}

	/**
	 * Gets the users with loan branch in their access branches.
	 *
	 * @author ManelLamloum
	 * @param loanBranchId the loan branch id
	 * @param usersDTO the users DTO
	 * @return the users with loan branch in their access branches
	 */
	@PostMapping("/get-users-with-loanBranch-in-their-accessBranches/{loanBranchId}")
	public List<UserDTO> getUsersWithLoanBranchInTheirAccessBranches(
			@PathVariable Integer loanBranchId, @RequestBody List<UserDTO> usersDTO) {

		return userService.getUsersWithLoanBranchInTheirAccessBranches(usersDTO, loanBranchId);
	}

	/**
	 * Gets the responsible of user.
	 *
	 * @author ManelLamloum
	 * @param usersDTO the users DTO
	 * @return the responsible of user
	 */
	@PostMapping("/get-responsible-of-user")
	public UserDTO getResponsibleOfUser(@RequestBody UserDTO usersDTO) {

		return userService.findResponsibleOfUser(usersDTO);
	}

	/**
	 * Update users responsible.
	 * 
	 * @author idridi
	 * @param usersDTOs the users DT os
	 * @return the list
	 */
	@PutMapping("/update-users-responsible")
	public List<UserDTO> updateUsersResponsible(@RequestBody List<UserDTO> usersDTOs) {

		return userService.updateUsersResponsible(usersDTOs);
	}

	/**
	 * Count user.
	 *
	 * @return the integer
	 */
	@GetMapping("/count-user")
	public Integer countUser() {

		return userService.countUser();

	}

	/**
	 * Gets the sumultanious user.
	 *
	 * @return the sumultanious user
	 */
	@GetMapping("/simultanious-user")
	public Long getSumultaniousUser() {

		return CommonLicenceVariable.currentSumiltaniousUser;

	}

}
