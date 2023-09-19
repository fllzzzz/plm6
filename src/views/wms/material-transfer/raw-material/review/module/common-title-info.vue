<template>
  <el-tag
    v-if="detail.transferType"
    v-parse-enum="{ e: transferTypeEnum, v: detail.transferType }"
    effect="plain"
    :type="detail.boolBorrowReturnNotSelf ? 'danger' : undefined"
    style="margin-right: 20px"
  />
  <span v-if="detail.borrowProject" class="borrow-info">
    <span class="label">借用项目：</span>
    <span
      class="project-ware-text"
      v-if="detail.direction.project"
      v-parse-project="{ project: detail.borrowProject, onlyShortName: true }"
      v-empty-text
    />
  </span>
  <span v-if="showDirection" class="direction-text">
    <span class="label">调拨至：</span>
    <span
      class="project-ware-text"
      v-if="detail.direction.project"
      v-parse-project="{ project: detail.direction.project, onlyShortName: true }"
      v-empty-text
    />
    <span class="public-ware-text" v-else>公共库</span>
    &nbsp;&nbsp;-&nbsp;&nbsp;
    <span v-if="detail.direction.factory">{{ detail.direction.factory.name }}</span>
    &nbsp;&nbsp;-&nbsp;&nbsp;
    <span v-if="detail.direction.warehouse">{{ detail.direction.warehouse.name }}</span>
  </span>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { transferTypeEnum } from '@/utils/enum/modules/wms'

const props = defineProps({
  detail: {
    type: Object,
    default: () => {
      return {
        direction: {}
      }
    }
  }
})

const showDirection = computed(() => {
  return props.detail.direction && props.detail.transferType !== transferTypeEnum.RETURN_PARTY_A.V
})
</script>

<style lang="scss" scoped>
.label {
  color:#333;
}
.direction-text {
  font-weight: bold;
  font-size: 13px;
}
.project-ware-text {
  color: darkgoldenrod;
}
.public-ware-text {
  color: brown;
}
.borrow-info {
  font-weight: bold;
  font-size: 13px;
  margin-right: 20px;
}
</style>
