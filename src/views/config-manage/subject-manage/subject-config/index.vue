<template>
  <div class="app-container">
    1111
  </div>
</template>

<script setup>
</script>

<style lang="scss" scoped>
.card-header {
  .el-button {
    padding: 5px;
  }
  .el-button+.el-button {
    margin-left: 5px;
  }
  .search-name {
    margin-top: 15px;
  }
  .search-type {
    margin-top: 15px;
    margin-left: 15px;
    // width:350px;
  }
  flex-wrap: wrap;
  .flex-rbc{
    width: 100%;
  }
}

.box-card {
  flex: 0 0 auto;
  ::-webkit-scrollbar {
    width: 6px;
    height: 6px;
  }
  ::-webkit-scrollbar-thumb {
    border-radius: 6px;
  }
}

.box-card+.box-card{
    margin-left: 10px;
}

.lv-one-card, .lv-two-card {
  width: 420px;
  height: 100%;
}

.lv-two-card {
  width: 520px;
  height: 100%;
}

.lv-three-card {
  width: 620px;
  height: 100%;
}

</style>
