<script>
export default {
  name: 'AuthRedirect',
  created() {
    // TODO:重定向
    const hash = window.location.search.slice(1)
    if (window.localStorage) {
      window.localStorage.setItem('x-admin-oauth-code', hash)
      window.close()
    }
  },
  render: function (h) {
    return h() // avoid warning message
  }
}
</script>
