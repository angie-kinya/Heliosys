$(document).ready(function() {
    // Auto-scroll chat container
    function scrollToBottom() {
      var chatContainer = $('#chat-chat_container');
      if (chatContainer.length) {
        chatContainer.scrollTop(chatContainer[0].scrollHeight);
      }
    }
    
    // Scroll to bottom when new message is added
    var observer = new MutationObserver(scrollToBottom);
    var chatContainer = document.getElementById('chat-chat_container');
    if (chatContainer) {
      observer.observe(chatContainer, { childList: true, subtree: true });
    }
    
    // Enter key to send message
    $(document).on('keypress', '#chat-user_input', function(e) {
      if (e.which === 13) {
        $('#chat-send_btn').click();
      }
    });
  });