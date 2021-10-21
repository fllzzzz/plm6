<template>
  <el-card shadow="always" style="width: 700px; margin-top: 20px">
      <template #header>
        <div class="clearfix">
          <span class="card-title">钢材入库配置</span>
          <common-button
          v-permission="permission.steelInboundEdit"
          :loading="submitLoading"
          :disabled="submitDisabled"
          show-tip
          size="mini"
          style="float: right"
          @click="submit"
        >
          保存
        </common-button>
        </div>
      </template>
      <el-form  v-loading="dataLoading" :disabled="dataLoading" :model="form" label-position="left" label-width="170px">
        <el-form-item :label="`车次钢材总重误差(${form.trainsDiffType === numOrPctEnum.NUMBER.V ? 'g' : '%'})`">
          <template v-slot:label>
            <span>车次钢材总重误差({{ form.trainsDiffType === numOrPctEnum.NUMBER.V ? 'g' : '%' }})</span>
            <!-- <el-tooltip
              effect="light"
              :content="`可配置一车次钢材的允许最大误差（可以设置百分比或固定重量（g），百分比的重量为，入库钢板的理论重量*百分比值）。\n当入库的计量方式为“磅计”（即实际重量）时，钢材的总重量与“当前车过磅重量”的差值（绝对值）超过该误差，将无法办理入库`"
              placement="top"
            >
              <div style="display: inline-block">
                <i class="el-icon-info" />
              </div>
            </el-tooltip> -->
          </template>
          <common-radio-button v-model:value="form.trainsDiffType" :options="numOrPctEnum" type="enum" size="small" />
          <el-input-number
            v-model="form.trainsDiff"
            :max="999999999999"
            controls-position="right"
            size="small"
            placeholder="请输入允许的误差值"
            style="width: 200px"
            :min="0"
            :precision="0"
          />
        </el-form-item>
        <el-form-item label="重量允许误差(%)">
          <template v-slot:label>
            <span>单件钢材重量误差({{ form.steelDiffType === numOrPctEnum.NUMBER.V ? 'g' : '%' }})</span>
            <el-tooltip
              effect="light"
              :content="`可配置单件钢材的允许最大误差（可以设置百分比或固定重量（g），百分比的重量为，入库钢板的理论重量*百分比值）。\n当入库的计量方式为“磅计”时，入库时钢材所输入的重量（过磅重量）与“钢材的理论重量”的差值（绝对值）超过该误差，办理时会发出预警。`"
              placement="top"
            >
              <div style="display: inline-block">
                <i class="el-icon-info" />
              </div>
            </el-tooltip>
          </template>
          <common-radio-button v-model:value="form.steelDiffType" :options="numOrPctEnum" type="enum" size="small" />
          <el-input-number
            v-model="form.steelDiff"
            :max="999999999999"
            controls-position="right"
            size="small"
            placeholder="请输入允许的误差值"
            style="width: 200px"
            :min="0"
          />
        </el-form-item>
      </el-form>
    </el-card>
</template>

<script setup>
import { ElNotification } from 'element-plus'

import { getInboundSteelConf, setInboundSteelConf } from '@/api/config/wms/base'

import { numOrPctEnum } from '@enum-ms/common'
import { reactive, ref, onMounted, inject, computed } from 'vue'
import { isObjectValueEqual } from '@/utils/data-type/object'
const permission = inject('permission')

// 数据源
let dataSource = {
  // 金额填写方式
  amountFillWay: undefined,
  // 工厂填写方式
  factoryFillWay: undefined
}
// 表单
const form = reactive(dataSource)

// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

// 未修改时，禁止点击保存按钮
const submitDisabled = computed(() => isObjectValueEqual(form, dataSource))

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
  } catch (error) {
    console.log('获取入库钢材配置', error)
  } finally {
    dataLoading.value = false
  }
}

// 保存数据
async function submit() {
  try {
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
</script>
