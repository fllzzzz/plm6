<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">钢材入库配置</span>
        <common-button
          v-permission="permission.steelInboundEdit"
          :loading="submitLoading"
          :disabled="submitDisabled"
          show-tip
          size="mini"
          type="primary"
          style="float: right"
          @click="submit"
        >
          保存
        </common-button>
      </div>
    </template>
    <el-form ref="formRef" v-loading="dataLoading" :disabled="formDisabled" :model="form" :rules="rules" label-position="left" label-width="170px">
      <el-form-item class="form-tip-item" prop="trainsDiffType">
        <template #label>
          <span>车次钢材总重误差({{ form.trainsDiffType === numOrPctEnum.NUMBER.V ? 'g' : '%' }})</span>
        </template>
        <div class="flex-r">
          <common-radio-button v-model="form.trainsDiffType" :options="numOrPctEnum.ENUM" type="enum" size="small" />
          <el-input-number
            v-model="form.trainsDiff"
            :max="form.trainsDiffType === numOrPctEnum.NUMBER.V ? 999999999999 : 100"
            controls-position="right"
            size="small"
            placeholder="请输入允许的误差值"
            style="width: 200px;margin-left: 10px;"
            :min="0"
            :precision="0"
          />
        </div>
      </el-form-item>
      <el-form-item>
        <span class="form-item-tip">可配置一车次钢材的最大误差。【入库钢材的总重量】与【当前车过磅重量】的差值（绝对值）超过该误差，将无法提交入库申请。</span>
        <span class="form-item-tip">固定重量(g)：误差不可超 过固定重量；</span>
        <span class="form-item-tip">百分比(%)：误差不可超 过车次总重量*百分比。</span>
      </el-form-item>
      <el-form-item class="form-tip-item" label="重量允许误差(%)" prop="steelDiffType">
        <template #label>
          <span>单件钢材重量误差({{ form.steelDiffType === numOrPctEnum.NUMBER.V ? 'g' : '%' }})</span>
        </template>
        <div class="flex-r">
          <common-radio-button v-model="form.steelDiffType" :options="numOrPctEnum.ENUM" type="enum" size="small" />
          <el-input-number
            v-model="form.steelDiff"
            :max="form.steelDiffType === numOrPctEnum.NUMBER.V ? 999999999999 : 100"
            controls-position="right"
            size="small"
            placeholder="请输入允许的误差值"
            style="width: 200px;margin-left: 10px;"
            :min="0"
          />
        </div>
      </el-form-item>
      <el-form-item>
        <span class="form-item-tip">可配置单件钢材的最大误差。【钢材的入库重量】与【钢材的理论重量】的差值（绝对值）超过该误差，办理时将发出预警（可提交入库申请）。</span>
        <span class="form-item-tip">固定重量(g)：误差不可超过 固定重量；</span>
        <span class="form-item-tip">百分比(%)：误差不可超过 入库钢材的理论重量*百分比。</span>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { ElNotification } from 'element-plus'

import { getInboundSteelConf, setInboundSteelConf } from '@/api/config/wms/base'

import { numOrPctEnum } from '@enum-ms/common'
import { reactive, ref, onMounted, inject, computed } from 'vue'
import { isObjectValueEqual } from '@data-type/object'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
const permission = inject('permission')

// 数据源
let dataSource = {
  // 车次重量差值
  trainsDiff: undefined,
  // 车次重量差值类型（g 或 %）
  trainsDiffType: undefined,
  // 单件钢材差值
  steelDiff: undefined,
  // 差值类型（g 或 %）
  steelDiffType: undefined
}

// from-dom
const formRef = ref()

// 表单
const form = reactive(dataSource)

// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

// 未修改时，禁止点击保存按钮
const submitDisabled = computed(() => isObjectValueEqual(form, dataSource))
const formDisabled = computed(() => dataLoading.value || submitLoading.value)

// 表单校验
const rules = reactive({
  trainsDiffType: [
    { validator: validateTrainsDiff }
  ],
  steelDiffType: [
    { validator: validateSteelDiff }
  ]
})

onMounted(() => {
  fetchData()
})

// 加载数据
async function fetchData() {
  dataLoading.value = true
  try {
    const res = await getInboundSteelConf()
    dataSource = res
    Object.assign(form, res)
    useWatchFormValidate(formRef, form)
  } catch (error) {
    console.log('获取入库钢材配置', error)
  } finally {
    dataLoading.value = false
  }
}

// 保存数据
async function submit() {
  try {
    const passed = await formRef.value.validate()
    if (!passed) return
    submitLoading.value = true
    await setInboundSteelConf(form)
    ElNotification({
      title: '入库钢材配置设置成功',
      type: 'success',
      duration: 2500
    })
    fetchData()
    // TODO:更新配置
    // await store.dispatch('config/fetchConfigInfo')
  } catch (error) {
    ElNotification({
      title: '基础钢材配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}

// 表单校验
function validateSteelDiff(rule, value, callback) {
  const flag = value === numOrPctEnum.PERCENTAGE.V && form.steelDiff > 100
  if (flag) {
    callback(new Error('误差百分比不可超过100'))
  }
  callback()
}

function validateTrainsDiff(rule, value, callback) {
  const flag = value === numOrPctEnum.PERCENTAGE.V && form.trainsDiff > 100
  if (flag) {
    callback(new Error('误差百分比不可超过100'))
  }
  callback()
}
</script>
