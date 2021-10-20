<template>
  <div class="logo-container">
    <img class="logo" :src="logo">
    <!-- <div class="logo-simple">
      <span>初鸣智造</span>
    </div>
    <div class="logo-detail">
      <span>钢结构生产执行管理系统</span>
      <span>Steel Structure Execution System</span>
    </div> -->
  </div>
</template>

<script>
import { mapGetters } from 'vuex'
export default {
  name: 'Logo',
  computed: {
    ...mapGetters(['logo'])
  }
}
</script>

<style lang="scss" scoped>
$bg:transparent;
.logo-container {
    line-height: 1.5;
    background: $bg;
    color: white;
    display: flex;
    flex-direction: row;
    justify-content: flex-start;
    align-items: center;
    height:40px;
    width: 490px;
    :nth-child(n) {
        margin-right: 5px;
    }
    .logo{
      height: 60px;
    }
    // .logo-simple {
    //     font-size:36px;
    //     display: flex;
    //     flex-direction: row;
    //     justify-content: center;
    //     align-items: center;
    // }
    // .logo-detail {
    //     display: flex;
    //     flex-direction: column;
    //     justify-content: flex-start;
    //     align-items: flex-start;
    //     height: 37px;
    //     :nth-child(1) {
    //         font-size: 14px;
    //         letter-spacing: 5px;
    //     }
    //     :nth-last-child(1) {
    //         font-size: 13px;
    //     }
    // }
}
</style>
