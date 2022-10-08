<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">甲供材料借用归还配置</span>
        <common-tip-button
          v-permission="permission.partyABorrowReturnEdit"
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
    </template>
    <el-form
      ref="formRef"
      v-loading="dataLoading"
      :disabled="formDisabled"
      :model="form"
      label-position="left"
      label-width="170px"
    >
      <el-form-item label="可以从非借用项目归还">
        <el-switch
          v-model="form.boolReturnByOtherProject"
          :active-value="whetherEnum.TRUE.V"
          :inactive-value="whetherEnum.FALSE.V"
          class="drawer-switch"
        />
      </el-form-item>
      <el-form-item>
        <span class="form-item-tip">情景：项目B借用 “甲供项目A” 的材料</span>
        <span class="form-item-tip">1.开启：可将项目库中“非项目B的材料（如项目C）”归还到 “甲供项目A”。</span>
        <span class="form-item-tip">2.关闭：只可将“项目B或公共库的材料”归还到 “甲供项目A”。</span>
      </el-form-item>
      <el-form-item label="钢板-边长度误差(mm)">
        <common-input-number v-model="form.steelPlateSideLengthDiff" controls-position="right" :precision="0" :min="0" :max="100000" style="width: 200px" />
      </el-form-item>
      <el-form-item label="型材-长度误差(mm)">
        <common-input-number v-model="form.sectionSteelLengthDiff" controls-position="right" :precision="0" :min="0" :max="100000" style="width: 200px" />
      </el-form-item>
      <!-- 目前按长度还，不需要该配置，若按照卷或重量还则需要对应配置-->
      <!-- <el-form-item label="钢卷-长度误差(mm)">
        <common-input-number v-model="form.steelCoilLengthDiff" controls-position="right" :precision="0" :min="0" :max="100000" style="width: 200px" />
      </el-form-item> -->
      <el-form-item>
        <span class="form-item-tip">
          可归还列表中不会检索出误差范围外的钢材。
        </span>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getPartyABorrowReturnConf, setPartyABorrowReturnConf } from '@/api/config/wms/base'
import { ref, onMounted, inject, computed } from 'vue'
import { whetherEnum } from '@enum-ms/common'
import { deepClone } from '@/utils/data-type'
import { isObjectValueEqual } from '@data-type/object'

import useRefreshStore from '@/composables/store/use-refresh-store'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  boolReturnByOtherProject: false, // 可从非借用项目归还
  steelPlateSideLengthDiff: undefined, // 钢板-长宽长度误差(mm)
  sectionSteelLengthDiff: undefined, // 型材-长度误差(mm)
  steelCoilLengthDiff: undefined // 钢卷-长度误差(mm)
})

// from-dom
const formRef = ref()

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
    const res = await getPartyABorrowReturnConf()
    form.value = deepClone(res)
    dataSource.value = res
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
    await setPartyABorrowReturnConf(form.value)
    ElNotification({
      title: '入库钢材配置设置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
    useRefreshStore('wmsConfig')
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
