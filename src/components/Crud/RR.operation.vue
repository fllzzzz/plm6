<!--搜索与重置-->
<template>
    <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="crud.toQuery">搜索</common-button>
    <common-button
      v-if="props.resetBtn"
      class="filter-item"
      size="mini"
      type="warning"
      icon="el-icon-refresh-left"
      @click.stop="crud.resetQuery()"
    >
      重置
    </common-button>
</template>
<script setup>
import { defineProps, inject } from 'vue'

const props = defineProps({
  resetBtn: {
    type: Boolean,
    default: true
  }
})

const crud = inject('crud')
</script>
