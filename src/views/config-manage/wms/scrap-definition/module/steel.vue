<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">钢材废料定义</span>
        <common-tip-button
          v-permission="permission.steelScrapDefinitionEdit"
          :loading="submitLoading"
          :disabled="submitDisabled"
          show-tip
          size="mini"
          type="primary"
          style="float: right"
          @click="submit"
        >
          保存
        </common-tip-button>
      </div>
      <!-- <span class="tip">出库：小于限定的尺寸，必须全部出库</span> -->
      <span class="tip">出库/入库/还库：小于限定的尺寸，直接进入废料列表</span>
    </template>
    <el-form v-loading="dataLoading" :disabled="formDisabled" :model="form" label-position="left" label-width="160px">
      <el-form-item label="钢板-最短边长度(mm)">
        <el-input-number v-model="form.steelPlateShortestSideMinLength" controls-position="right" :precision="0" :min="0" :max="100000" style="width: 200px" />
      </el-form-item>
      <el-form-item label="型钢-长度(mm)">
        <el-input-number v-model="form.sectionSteelMinLength" controls-position="right" :precision="0" :min="0" :max="100000" style="width: 200px" />
      </el-form-item>
      <el-form-item label="钢卷-长度(mm)">
        <el-input-number v-model="form.steelCoilMinLength" controls-position="right" :precision="0" :min="0" :max="100000" style="width: 200px" />
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getSteelScrapDefinitionConf, setSteelScrapDefinitionConf } from '@/api/config/wms/scrap-definition'
import { ref, onMounted, inject, computed } from 'vue'
import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  // 钢板最短边长度
  steelPlateShortestSideMinLength: undefined,
  // 型材最小长度
  sectionSteelMinLength: undefined,
  // 钢卷最小长度
  steelCoilMinLength: undefined
})
// 表单
const form = ref(dataSource.value)

// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

// 未修改时，禁止点击保存按钮
const submitDisabled = computed(() => isObjectValueEqual(form.value, dataSource.value))
const formDisabled = computed(() => dataLoading.value || submitLoading.value)

onMounted(() => {
  fetchData()
})

// 加载数据
async function fetchData() {
  dataLoading.value = true
  try {
    const res = await getSteelScrapDefinitionConf()
    form.value = deepClone(res)

    dataSource.value = res
  } catch (error) {
    console.log('wms基础配置', error)
  } finally {
    dataLoading.value = false
  }
}

// 保存数据
async function submit() {
  try {
    submitLoading.value = true

    await setSteelScrapDefinitionConf(form.value)
    ElNotification({
      title: '钢材废料定义设置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
    // TODO:更新配置
    // await store.dispatch('config/fetchConfigInfo')
  } catch (error) {
    ElNotification({
      title: '钢材废料定义设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.tip {
  display: block;
  margin-top: 8px;
  line-height: 20px;
  font-size: 13px;
  color: #e6a23cc2;
}
</style>
