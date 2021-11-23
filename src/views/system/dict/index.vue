<template>
  <div class="app-container">
    <el-row :gutter="10">
      <el-col :xs="24" :sm="24" :md="24" :lg="10" :xl="10" style="margin-bottom: 10px">
        <dictConfig @click-line="handleChangeline" ref="dicRef"/>
      </el-col>
      <el-col :xs="24" :sm="24" :md="24" :lg="14" :xl="14">
        <el-card class="box-card team-card">
          <template v-slot:header class="clearfix card-header">
            <span style="margin-right:5px;">详情</span>
            <el-tag v-if="currentLine.remark" size="medium">{{currentLine.remark}}</el-tag>
            <common-button
              v-if="currentLine.name && dictDetailRef && checkPermission(dictDetailRef.permission.add)"
              size="mini"
              style="float: right; padding: 6px 10px; margin-bottom: 0px"
              type="primary"
              icon="el-icon-plus"
              @click="dictDetailRef.toAdd"
            >
              新增
            </common-button>
          </template>
          <dictDetailConfig ref="dictDetailRef" :line="currentLine" />
        </el-card>
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { reactive, ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import dictConfig from './dict'
import dictDetailConfig from './dict-detail'
const dicRef = ref()
const dictDetailRef = ref()
let currentLine = reactive({})

function handleChangeline(val) {
  if (val) {
    currentLine = Object.assign(currentLine, val)
  }
}
</script>
