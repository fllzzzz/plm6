<template>
  <div class="app-container">
    <img src="@/assets/black-bg.jpg" class="img-bg" />
    <div class="title-drawer">
      {{ `${component.name} ${component.serialNumber}` }}
    </div>
    <span class="subtitle"> 基础信息 </span>
    <div class="info">
      <span>名称：{{ component.name }}</span>
      <span>编号：{{ component.serialNumber }}</span>
      <span>板型：{{ component.plateType }}</span>
      <span>颜色：{{ component.color }}</span>
      <span>厚度：{{ component.thickness }}{{ component.thickness ? ' mm' : '' }}</span>
      <span>长度：{{ component.length }}{{ component.length ? ' mm' : '' }}</span>
      <span>有效宽度：{{ component.effectiveWidth }}{{ component.effectiveWidth ? ' mm' : '' }}</span>
    </div>
    <span class="subtitle"> 使用部位 </span>
    <div class="info">
      <span>项目：{{ component.project?.shortName }}</span>
      <span v-if="showMonomer">单体：{{ component.monomer?.name }}</span>
      <span v-if="showArea">区域：{{ component.area?.name }}</span>
    </div>
    <template v-if="showProductionLine">
      <span class="subtitle"> 生产信息 </span>
      <div class="info">
        <template v-if="showProductionLine">
          <span>工厂：{{ component.factory?.name }}</span>
          <span>生产线：{{ component.productionLine?.name }}</span>
        </template>
        <!-- <span v-if="manufacturerName">制造商：{{ manufacturerName  }}</span> -->
        <!-- <span>任务数量：{{ component.taskQuantity  }}</span> -->
      </div>
    </template>
  </div>
</template>

<script setup>
import { fetchEnclosure as fetchInfo } from '@/api/qr-scan-info'
import { ref } from 'vue'
import { useRoute } from 'vue-router'
import { ElLoading } from 'element-plus'

import { specialPath } from '@/settings/config'

const route = useRoute()

const component = ref({
  name: '',
  serialNumber: '',
  plateType: '',
  color: '',
  thickness: '',
  area: '',
  length: '',
  effectiveWidth: ''
})

const id = route.query.id
const factoryId = route.query.factoryId
const taskId = route.query.taskId
const showProductionLine = ref(Boolean(+route.query.sl))
const showArea = ref(Boolean(+route.query.sa))
const showMonomer = ref(Boolean(+route.query.sm))
const params = {
  id,
  factoryId,
  taskId
}
const url = window.location.href.split(specialPath.QR_SCAN_ENCLOSURE_TASK)[0].split('/#')[0]
console.log(url)
fetch(url, params)

async function fetch(url, params) {
  const loading = ElLoading.service({
    lock: true,
    text: '正在加载围护信息',
    background: 'rgba(0, 0, 0, 0.7)',
    fullscreen: true
  })
  try {
    const res = await fetchInfo(url, params)
    component.value = res
    console.log(component)
  } catch (error) {
    console.log('获取围护信息失败', error)
  } finally {
    loading.close()
  }
}
</script>

<style lang="scss" scoped>
.app-container {
  .img-bg {
    border-style: none;
    position: fixed;
    width: 100%;
    height: 100%;
    top: 0;
    left: 0;
    z-index: -1;
  }
  color: #e8e7ad;
  width: 100%;
  height: 100%;
  // background: url('~@/assets/black-bg.jpg') fixed top center no-repeat;
  // background-size:1024px auto;
  .title-drawer {
    // border-top-left-radius: 10px;
    // border-bottom-right-radius: 10px;
    border-radius: 10px;
    // background: #ffba00;
    background: #ffe49b;
    height: 40px;
    line-height: 40px;
    text-align: center;
    font-size: 1.5rem;
    // color: white;
    color: #303133;
    box-sizing: border-box;
    padding: 0 10px;
    margin-bottom: 10px;
  }
  .subtitle {
    border-top-left-radius: 10px;
    border-bottom-right-radius: 10px;
    // background: rgb(247, 104, 104);
    background: #ff4949;
    display: inline-block;
    width: 50%;
    height: 32px;
    line-height: 32px;
    box-sizing: border-box;
    padding: 0 10px;
    margin: 10px 0 3px 0;
    color: white;
    font-size: 1.2rem;
  }
  .info {
    & > span {
      display: block;
      line-height: 24px;
      padding: 3px 0;
      font-size: 1.1rem;
    }
  }
}
</style>
