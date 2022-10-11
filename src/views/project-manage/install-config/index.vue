<template>
  <div class="app-container">
    <template v-if="isNotBlank(globalProject?.installReportMethod) && globalProject?.businessType===businessTypeEnum.INSTALLATION.V">
      <el-row :gutter="40">
        <el-col :span="8">
          <auxiliary-config />
        </el-col>
        <el-col :span="10">
          <project-config style="margin-bottom:20px" :globalProject="globalProject"/>
        </el-col>
      </el-row>
    </template>
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
  </div>
</template>

<script setup>
import { mapGetters } from '@/store/lib'

import { isNotBlank } from '@data-type/index'
import { businessTypeEnum } from '@enum-ms/contract'

import auxiliaryConfig from './module/auxiliary-config'
import projectConfig from './module/project'

const { globalProject } = mapGetters(['globalProject'])

</script>
