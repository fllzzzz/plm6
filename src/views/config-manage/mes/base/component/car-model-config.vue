<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">车型配置</span>
        <common-tip-button v-if="isEdit" :loading="submitLoading" show-tip size="mini" type="primary" style="float: right" @click="submit">
          保存
        </common-tip-button>
        <common-button
          v-permission="permission.driverFillConfigEdit"
          size="mini"
          type="primary"
          style="float: right"
          v-if="!isEdit"
          @click="handleEdit"
          >编辑</common-button
        >
        <common-button size="mini" type="warning" style="float: right" v-else @click="cancel">取消编辑</common-button>
      </div>
    </template>
    <common-table v-loading="dataLoading" :maxHeight="maxHeight" :data="carModels">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="name" label="车型">
        <template v-slot="scope">
          <el-input v-if="isEdit" v-model="scope.row.name" size="mini" placeholder="车型" style="width:100%;"></el-input>
          <span v-else>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="isEdit" label="操作" width="100" align="center">
        <template v-slot="scope">
          <common-button
            size="mini"
            v-if="scope.$index === carModels.length - 1"
            type="primary"
            icon="el-icon-plus"
            @click="add"
            style="padding: 6px"
          ></common-button>
          <common-button size="mini" type="danger" icon="el-icon-delete" @click="del(scope.$index)" style="padding: 6px"></common-button>
        </template>
      </el-table-column>
    </common-table>
  </el-card>
</template>

<script setup>
import { getCarModelConfig as getConfig, setCarModelConfig as setConfig } from '@/api/config/mes/base'
import { ref, onMounted, inject } from 'vue'

import { deepClone } from '@/utils/data-type'

import useMaxHeight from '@compos/use-max-height'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

const { maxHeight } = useMaxHeight({
  extraBox: ['.el-card__header'],
  wrapperBox: ['.app-container', '.el-card__body'],
  minHeight: 300
})
const isEdit = ref(false)
// 表单
const carModels = ref([])
const originCarModels = ref([])
// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

onMounted(() => {
  fetchData()
})

async function fetchData() {
  dataLoading.value = true
  try {
    const data = await getConfig()
    carModels.value = data.carModels.map(v => {
      return { name: v }
    })
    originCarModels.value = data.carModels.map(v => {
      return { name: v }
    })
  } catch (error) {
    console.log('获取车型配置', error)
  } finally {
    dataLoading.value = false
  }
}

function handleEdit() {
  if (carModels.value.length === 0) {
    add()
  }
  isEdit.value = !isEdit.value
}

function cancel() {
  carModels.value = deepClone(originCarModels.value)
  isEdit.value = false
}

function del(index) {
  carModels.value.splice(index, 1)
}

function add() {
  carModels.value.push({})
}

async function submit() {
  submitLoading.value = true
  try {
    const _list = carModels.value.map(v => v.name)
    await setConfig({ carModels: _list })
    ElNotification({
      title: '车型配置成功',
      type: 'success',
      duration: 2500
    })
    originCarModels.value = deepClone(carModels.value)
    isEdit.value = false
  } catch (error) {
    console.log('设置车型配置', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
