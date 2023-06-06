<template>
  <div class="app-container">
    <el-row :gutter="10" id="factory-and-workshop">
      <!-- 工厂列表 -->
      <el-col :xs="24" :sm="24" :md="24" :lg="10" :xl="10" style="margin-bottom: 10px">
        <factory-config @click-factory="handleChangeFactory" />
      </el-col>
      <!-- 车间列表 -->
      <el-col :xs="24" :sm="24" :md="24" :lg="14" :xl="14">
        <workshop-config :factory="curFactory" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { ref, provide } from 'vue'

import useMaxHeight from '@compos/use-max-height'

import factoryConfig from './factory/index'
import workshopConfig from './workshop/index'

const curFactory = ref()

const { maxHeight } = useMaxHeight({ wrapperBox: ['.app-container', '#factory-and-workshop'], extraBox: ['.head-container', '.el-card__header'], paginate: true, extraHeight: 55 })

provide('maxHeight', maxHeight)

function handleChangeFactory(val) {
  curFactory.value = val
}
</script>
