<template>
  <div class="head-container">
    <factory-select
      v-model:value="query.factoryId"
      class="filter-item"
      placeholder="可选择工厂"
      clearable
      style="width: 200px"
      @change="crud.toQuery"
    />
    <material-cascader
      check-strictly
      v-model="query.classifyId"
      separator=" > "
      show-all-levels
      clearable
      size="small"
      class="filter-item"
      style="width: 300px"
      placeholder="可选择/输入科目、编码搜索"
      @change="crud.toQuery"
    />
    <el-input
      v-model.trim="query.classifySpec"
      placeholder="可输入规格搜索"
      class="filter-item"
      style="width: 250px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <rrOperation />
    <crudOperation>
      <template #viewLeft>
        <common-button size="mini" type="warning" @click="openNotify">查看/设置预警通知人</common-button>
      </template>
    </crudOperation>
    <notify-others v-model="notifyVisible" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import materialCascader from '@comp-cls/material-cascader/index.vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation.vue'
import rrOperation from '@crud/RR.operation.vue'
import factorySelect from '@comp-base/factory-select.vue'
import notifyOthers from './notify-others.vue'

const defaultQuery = {
  factoryId: undefined, // 工厂id
  classifyId: undefined, // 科目id
  classifySpec: undefined // 科目规格
}

const { crud, query } = regHeader(defaultQuery)

const notifyVisible = ref(false)

function openNotify() {
  notifyVisible.value = true
}
</script>
