/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.websocket;

import java.util.Map;

import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.http.server.ServletServerHttpRequest;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.config.annotation.EnableWebSocketMessageBroker;
import org.springframework.web.socket.config.annotation.StompEndpointRegistry;
import org.springframework.web.socket.config.annotation.StompWebSocketEndpointRegistration;
import org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer;
import org.springframework.web.socket.server.support.DefaultHandshakeHandler;

/**
 * The Class WebSocketBrokerConfig.
 */
/**
 * {@link WebSocketBrokerConfig} class.
 *
 * @author idridi
 * @since 1.0.8
 */
@Configuration
@EnableWebSocketMessageBroker
public class WebSocketBrokerConfig implements WebSocketMessageBrokerConfigurer {

	/** The end point web socket. */
	@Value("${url.endpoint.websocket}")
	private String endPointWebSocket;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer#
	 * configureMessageBroker(org.springframework.messaging.simp.config.MessageBrokerRegistry)
	 */
	@Override
	public void configureMessageBroker(MessageBrokerRegistry registry) {

		// defines the access prefix to any controllers that clients can consume from our API
		registry.setApplicationDestinationPrefixes("/app");
		registry.setUserDestinationPrefix("/user");
		// defines the access prefix to the flow issued by the broker for clients wishing to
		// subscribe
		registry.enableSimpleBroker("/topic/", "/queue/");
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer#
	 * registerStompEndpoints(org.springframework.web.socket.config.annotation.
	 * StompEndpointRegistry)
	 */
	@Override
	public void registerStompEndpoints(StompEndpointRegistry registry) {

		// defines the entry point for the handshake between the client and the server.
		// This allows an open connection to be established between the two services
		StompWebSocketEndpointRegistration registration = registry.addEndpoint("/stomp");

		registration.setHandshakeHandler(new DefaultHandshakeHandler() {
			@SuppressWarnings("unused")
			public boolean beforeHandshake(ServerHttpRequest request, ServerHttpResponse response,
					WebSocketHandler webSocketHandler, Map<String, String> attributes)
					throws Exception {

				if (request instanceof ServletServerHttpRequest) {
					ServletServerHttpRequest servletRequest = (ServletServerHttpRequest) request;
					HttpSession session = servletRequest.getServletRequest().getSession();
					attributes.put("sessionId", session.getId());
				}
				return true;
			}
		});
		registration.setAllowedOrigins(endPointWebSocket).withSockJS();
	}
}
