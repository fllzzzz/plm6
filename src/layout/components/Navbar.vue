<template>
  <div class="navbar">
    <hamburger  style="padding: 0 15px;" :is-active="sidebar.opened" class="hamburger-container" @toggleClick="toggleSideBar" />

    <breadcrumb class="breadcrumb-container" />
    <head-switch-project />
    <div class="right-menu">
      <template v-if="device !== 'mobile'">
        <!-- <error-log class="errLog-container right-menu-item hover-effect" /> -->
        <!-- <screenfull class="right-menu-item hover-effect" /> -->
        <inventory-notify v-if="inventoryNotifyPerm" class="right-menu-item hover-effect" />
        <el-tooltip content="字体大小" effect="dark" placement="bottom">
          <span class="right-menu-item hover-effect"><size-select/></span>
        </el-tooltip>
      </template>
      <div class="right-menu-item hover-effect avatar-container avatar">
        <user-header :placement="'bottom'" />
      </div>
    </div>
  </div>
</template>

<script setup>
import { mapGetters } from '@/store/lib'
import { useStore } from 'vuex'
import Breadcrumb from '@comp/Breadcrumb/index.vue'
import Hamburger from '@comp/Hamburger/index.vue'
// import ErrorLog from '@comp/ErrorLog/index.vue'
// import Screenfull from '@comp/Screenfull'
import SizeSelect from '@comp/SizeSelect/index.vue'
import UserHeader from '@comp/UserHeader/index.vue'
import HeadSwitchProject from './SwitchProject'
import inventoryNotify from '@/components-system/wms/inventory-notify/index.vue'

const store = useStore()

const { sidebar, device, inventoryNotifyPerm } = mapGetters(['sidebar', 'device', 'inventoryNotifyPerm'])
/**
 * 切换SideBar
 */
function toggleSideBar() {
  store.dispatch('app/toggleSideBar')
}
</script>

<style lang="scss" scoped>
.navbar {
  height: 50px;
  overflow: hidden;
  position: relative;
  background: #fff;
  box-shadow: 0 1px 4px rgba(0, 21, 41, 0.08);

  .hamburger-container {
    line-height: 46px;
    height: 100%;
    float: left;
    cursor: pointer;
    transition: background 0.3s;
    -webkit-tap-highlight-color: transparent;

    &:hover {
      background: rgba(0, 0, 0, 0.025);
    }
  }

  .breadcrumb-container {
    float: left;
  }

  .errLog-container {
    display: inline-block;
    vertical-align: top;
  }

  .right-menu {
    float: right;
    height: 100%;
    line-height: 50px;

    &:focus {
      outline: none;
    }

    .avatar {
      margin-right: 0;
      display: inline-flex !important;
      flex-direction: row;
      align-items: center;
      justify-content: center;
    }

    .right-menu-item {
      display: inline-block;
      padding: 0 8px;
      height: 100%;
      font-size: 18px;
      color: #5a5e66;
      vertical-align: text-bottom;

      &.hover-effect {
        cursor: pointer;
        transition: background 0.3s;

        &:hover {
          background: rgba(0, 0, 0, 0.025);
        }
      }
    }
  }
}
</style>
