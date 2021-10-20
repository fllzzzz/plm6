<!--搜索与重置-->
<template>
  <span>
    <el-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="crud.toQuery">搜索</el-button>
    <el-button
      v-if="resetBtn"
      class="filter-item"
      size="mini"
      type="warning"
      icon="el-icon-refresh-left"
      @click.stop="crud.resetQuery()"
    >
      重置
    </el-button>
  </span>
</template>
<script>
export default {
  props: {
    crud: {
      type: Object,
      required: true
    },
    resetBtn: {
      type: Boolean,
      default: true
    },
    itemClass: {
      type: String,
      required: false,
      default: ''
    }
  }
}
</script>
