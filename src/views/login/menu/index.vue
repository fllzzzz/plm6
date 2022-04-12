<template>
  <div class="menus-container">
    <el-row :gutter="20" class="el-row-content">
      <el-col v-for="item in menus" :key="item.id" :xs="8" :sm="8" :md="8" :lg="6" :xl="6">
        <div class="menu-drawer" @click="goView(item)">
          <div class="menu-img">
            <svg-icon :icon-class="item.icon" class="icon" />
          </div>
          <span class="menu-name" v-text="item.name" />
        </div>
      </el-col>
      <el-col v-for="item in fixedMenus" :key="item.id" :xs="8" :sm="8" :md="8" :lg="6" :xl="6">
        <div class="menu-drawer" @click="goView(item)">
          <div class="menu-img">
            <svg-icon :icon-class="item.icon" class="icon" />
          </div>
          <span class="menu-name" v-text="item.name" />
        </div>
      </el-col>
    </el-row>
  </div>
</template>

<script>
import { mapGetters } from 'vuex'

export default {
  name: 'LoginMenuComponent',
  data() {
    return {
      fixedMenus: [
        // { name: '关于我们', id: -1, icon: 'about-us' }
      ]
    }
  },
  computed: {
    ...mapGetters(['menus', 'currentMenu'])
  },
  methods: {
    async goView(view) {
      this.$emit('replace', view.redirect)
      this.$router.push({ path: view.redirect })
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
$menuBg: rgba(0, 0, 0, 0.4);
$menuTextColor: #f3f3f3;
.menus-container {
  z-index: 5;
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  width: 1000px;
  margin: 150px auto;
  display: flex;
  flex-flow: row wrap;
  justify-content: center;
  align-items: center;

  .el-row-content {
    display: flex;
    flex-flow: row wrap;
    justify-content: center;
    align-items: center;
    width: 100%;
  }

  .menu-drawer {
    width: 100%;
    height: 150px;
    margin: 10px;
    background: $menuBg;
    color: $menuTextColor;
    border-radius: 5px;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    cursor: pointer;

    // transform: scale(1);
    transition: all 0.3s ease-in-out 0s;

    &:hover {
      transform: scale(1.1);
      background: rgba(7, 18, 39, 0.7);
      // .menu-img {
      //    .icon {
      //     left: -100px;
      //   }
      // }
    }
    .menu-name {
      color: #d3faff;
    }
    .menu-img {
      width: 100%;
      margin-bottom: 10px;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      overflow: hidden;

      .icon {
        width: 100%;
        height: 70px;
        color: #d3faff;
        // position: relative;
        // left: 0;
        // margin-bottom: .1rem;
        // filter: drop-shadow(#ffffff 100px 0 );
        // border-left: 30px solid transparent;
        // border-right: 30px solid transparent;
      }
    }
  }
}
</style>
