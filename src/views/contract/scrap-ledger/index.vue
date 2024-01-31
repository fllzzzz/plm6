<template>
  <div class="app-container">
    <mHeader @wayChange="wayChange" @dateChange="dateChange" @typeChange="typeChange" />
    <buy-query v-if="isTable === 1" :queryId="isTable" :startDate="startDate" :endDate="endDate" :scrapType="scrapType" />
    <date-query v-else :queryId="isTable" :startDate="startDate" :endDate="endDate" :scrapType="scrapType" />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import mHeader from './module/header.vue'
import dateQuery from './module/date-query.vue'
import buyQuery from './module/buy-query.vue'
import moment from 'moment'

const isTable = ref(1)
// const date = ref([moment().startOf('month').valueOf(), moment().valueOf()])
const scrapType = ref(undefined)
const startDate = ref(moment().startOf('month').valueOf())
const endDate = ref(moment().valueOf())

const wayChange = (v) => {
  isTable.value = v
  scrapType.value = undefined
}

function dateChange(start, end) {
  startDate.value = start
  endDate.value = end
}

const typeChange = (v) => {
  scrapType.value = v
}

</script>
